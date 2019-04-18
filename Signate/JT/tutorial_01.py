import pandas as pd
import os
import json
from matplotlib import pyplot as plt

master = pd.read_csv('master.tsv', sep='\t')

# 223, 3
print(master.shape)

master.head(5)

unique_id = master['category'].unique()
unique_brand = master['brand'].unique()

# 22ブランド
print('total:', len(unique_brand))
print(unique_brand)

# 223銘柄
print('total:', len(unique_id))
print(unique_id)

annotations = os.listdir('train_annotations')
print(annotations)

category_data = {}
for annotation in annotations:
    with open(os.path.join('train_annotations', annotation)) as f:
        data = json.load(f)
    labels = data['labels']
    for label in labels:
        category = label['category']
        if category not in category_data:
            category_data[category] = {'num':0, 'acc_area':0}
        category_data[category]['num'] += 1
        category_data[category]['acc_area'] += (label['box2d']['x2']-label['box2d']['x1'])*(label['box2d']['y2']-label['box2d']['y1'])

category_df = pd.DataFrame(category_data).T
print(category_df.head(5))
category_df['area_mean'] = category_df['acc_area']/category_df['num']
print(category_df[['num', 'area_mean']].describe())

fig, axes = plt.subplots(nrows = 1, ncols=2, figsize=(30,10))
modes = ['num','area_mean']
for i,mode in enumerate(modes):
    axes[i].hist(list(category_df[mode]), 20)
    axes[i].set_title(mode, fontsize=30)
    axes[i].set_ylabel('count', fontsize=30)

category_df = category_df.reset_index()[['index', 'num', 'area_mean']]
category_df.columns = ['category', 'num', 'area_mean']
master = pd.merge(master, category_df)

summary_master = master.groupby('brand')[['num']].apply(sum).sort_values('num', ascending=False)

print('total:', summary_master['num'].sum())


# 可視化
import numpy as np
import chainer
from PIL import Image
from chainercv.chainer_experimental.datasets.sliceable import GetterDataset
from chainercv.utils import read_image
from chainercv.visualizations import vis_bbox
from pylab import rcParams

rcParams['figure.figsize'] = 12,12
plt.rcParams['font.family'] = 'AppleGothic'


class BboxDataset(GetterDataset):
    def __init__(self, img_dir, annotation_dir, categories, img_ext='.jpg', annotation_ext='.json'):
        super(BboxDataset, self).__init__()

        self.names = [i.split('.')[0] for i in os.listdir(img_dir)]
        self.img_dir = img_dir
        self.annotation_dir = annotation_dir
        self.categories = categories
        self.img_ext = img_ext
        self.annotation_ext = annotation_ext
        self.add_getter('img', self.get_image)
        self.add_getter(('bbox', 'label'), self.get_annotation)

    def __len__(self):
        return len(self.names)

    def get_image(self, i):
        name = self.names[i]
        img_path = os.path.join(self.img_dir, name + self.img_ext)
        img = read_image(img_path, color=True)

        return img

    def get_annotation(self, i):
        name = self.names[i]
        annotation_path = os.path.join(self.annotation_dir, name + self.annotation_ext)
        with open(annotation_path) as f:
            annotation = json.load(f)
        bbox = []
        label = []

        for l in annotation['labels']:
            category = str(l['category'])
            if category in self.categories:
                bb = l['box2d']
                bbox.append([bb['y1'], bb['x1'], bb['y2'], bb['x2']])
                label.append(self.categories.index(category))

        bbox = np.array(bbox).astype(np.float32)
        label = np.array(label).astype(np.int32)

        return bbox, label

categories = [str(i) for i in master['category']]
data = BboxDataset('train_images', 'train_annotations', categories)

index_num = 20
print(data.names[index_num])
img, bbox, label = data[index_num]
vis_bbox(img, bbox, label, label_names=categories)
plt.show()

brands = list(master['brand'])
vis_bbox(img, bbox, label, label_names=brands)
plt.show()

brand = master.loc[set(master[master['brand'] == 'ﾋﾟｱﾆｯｼﾓ'].index).intersection(set(label))]
imgs = []
for l, b in zip(label, bbox):
    if l in brand.index:
        imgs.append((brand.at[l, 'category'],
                         img[:, int(b[0]):int(b[2]) + 1, int(b[1]):int(b[3]) + 1].transpose(1, 2, 0).astype(np.uint8)))

fig, axes = plt.subplots(nrows = len(imgs), ncols=2, figsize=(5,4*(len(imgs))))
for i,image in enumerate(imgs):
    master_image = read_image(os.path.join('master_images', str(image[0])+'.jpg')).transpose((1,2,0)).astype(np.uint8)
    axes[i,0].imshow(master_image)
    axes[i,0].set_title('{}'.format(image[0]), fontsize=20)

    axes[i,1].imshow(image[1])
    axes[i,1].set_title('cropped image', fontsize=20)


# Modeling ssd300

from chainercv.links.model.ssd import multibox_loss
from chainercv.links import SSD300
class MultiboxTrainChain(chainer.Chain):
    def __init__(self, model, alpha=1, k=3):
        super(MultiboxTrainChain, self).__init__()
        with self.init_scope():
            self.model = model
        self.alpha = alpha
        self.k = k

    def __call__(self, imgs, gt_mb_locs, gt_mb_labels):
        mb_locs, mb_confs = self.model(imgs)
        loc_loss, conf_loss = multibox_loss(
            mb_locs, mb_confs, gt_mb_locs, gt_mb_labels, self.k)
        loss = loc_loss * self.alpha + conf_loss

        chainer.reporter.report(
            {'loss': loss, 'loss/loc': loc_loss, 'loss/conf': conf_loss},
            self)

        return loss
eval_categories = ('tabacco',)
model_detector = SSD300(n_fg_class=len(eval_categories), pretrained_model = 'imagenet')
model_detector.nms_thresh = 0.5
model_detector.score_thresh = 0.5
detector_train_chain = MultiboxTrainChain(model_detector)

# 入力用データ作成
import copy
from chainer.datasets import TransformDataset
from chainercv import transforms
from chainercv.links.model.ssd import random_crop_with_bbox_constraints
from chainercv.links.model.ssd import random_distort
from chainercv.links.model.ssd import resize_with_random_interpolation
class Transform():
    def __init__(self, coder, size, mean):
        # to send cpu, make a copy
        self.coder = copy.copy(coder)
        self.coder.to_cpu()

        self.size = size
        self.mean = mean

    def __call__(self, in_data):
        img, bbox, label = in_data

        # 1. Color augmentation
        img = random_distort(img)

        # 2. Random expansion
        if np.random.randint(2):
            img, param = transforms.random_expand(
                img, fill=self.mean, return_param=True)
            bbox = transforms.translate_bbox(
                bbox, y_offset=param['y_offset'], x_offset=param['x_offset'])

        # 3. Random cropping
        img, param = random_crop_with_bbox_constraints(
            img, bbox, return_param=True)
        bbox, param = transforms.crop_bbox(
            bbox, y_slice=param['y_slice'], x_slice=param['x_slice'],
            allow_outside_center=False, return_param=True)
        label = label[param['index']]

        # 4. Resizing with random interpolatation
        _, H, W = img.shape
        img = resize_with_random_interpolation(img, (self.size, self.size))
        bbox = transforms.resize_bbox(bbox, (H, W), (self.size, self.size))

        # 5. Random horizontal flipping
        img, params = transforms.random_flip(
            img, x_random=True, return_param=True)
        bbox = transforms.flip_bbox(
            bbox, (self.size, self.size), x_flip=params['x_flip'])

        # Preparation for SSD network
        img -= self.mean
        mb_loc, mb_label = self.coder.encode(bbox, label)

        return img, mb_loc, mb_label
os.makedirs('train_images_0', exist_ok=True)
os.makedirs('train_annotations_0', exist_ok=True)
os.makedirs('val_images', exist_ok=True)
os.makedirs('val_annotations', exist_ok=True)
def crop_image(src_img_dir, img_fname, src_annotation_dir, annotation_fname, dst_img_dir, dst_annotation_dir, stride=300, crop_size=600, buffer=30):
    image = Image.open(os.path.join(src_img_dir, img_fname))
    image_array = np.array(image)
    with open(os.path.join(src_annotation_dir, annotation_fname)) as f:
        annotation = json.load(f)
    bboxes = pd.DataFrame([bb['box2d'] for bb in annotation['labels']])
    count = 0
    for i in range(0,image_array.shape[0],stride):
        for j in range(0, image_array.shape[1],stride):
            inside = bboxes[(bboxes['x1']>=j-buffer)&(bboxes['y1']>=i-buffer)&(bboxes['x2']<=j+crop_size+buffer)&(bboxes['y2']<=i+crop_size+buffer)]
            if len(inside)>0:
                cropped_image = image_array[i:i+crop_size,j:j+crop_size,:]
                cropped_annotation={}
                cropped_annotation['labels'] = [{'box2d':{'x1':int(r['x1'])-j,
                                                          'y1':int(r['y1'])-i,
                                                          'x2':int(r['x2'])-j,
                                                          'y2':int(r['y2'])-i},
                                                 'category': 'tabacco'} for r in inside.to_dict('record')]
                img_name = img_fname.split('.')[0]+'_'+str(count)+'.jpg'
                annotation_name = annotation_fname.split('.')[0]+'_'+str(count)+'.json'
                Image.fromarray(cropped_image).save(os.path.join(dst_img_dir, img_name))
                with open(os.path.join(dst_annotation_dir, annotation_name), 'w') as f:
                    json.dump(cropped_annotation,f)
                count += 1
count = 0
for train_images_file in os.listdir('train_images'):
    annotation_file_name =  os.path.splitext(train_images_file)[0]+'.json'
    if count < 175:
        crop_image('train_images', train_images_file, 'train_annotations', annotation_file_name, 'train_images_0', 'train_annotations_0')
    else:
        crop_image('train_images', train_images_file, 'train_annotations', annotation_file_name, 'val_images', 'val_annotations')
    count+=1
train_data = BboxDataset('train_images_0', 'train_annotations_0', eval_categories)
val_data = BboxDataset('val_images', 'val_annotations', eval_categories)
transformed_train_data = TransformDataset(train_data, Transform(model_detector.coder, model_detector.insize, model_detector.mean))


# train
from chainer.optimizer_hooks import WeightDecay
from chainercv.links.model.ssd import GradientScaling
from chainer import training
from chainer.training import extensions, triggers
from chainercv.extensions import DetectionVOCEvaluator

gpu = 1
batchsize = 20
num_epochs = 20
train_iter = chainer.iterators.SerialIterator(transformed_train_data, batchsize)
val_iter = chainer.iterators.SerialIterator(val_data, batchsize, repeat=False, shuffle=False)
if gpu:
    gpu_id = 0
    model_detector.to_gpu()
else:
    gpu_id = -1
optimizer = chainer.optimizers.MomentumSGD(lr=0.0005)
optimizer.setup(detector_train_chain)

for param in detector_train_chain.params():
    if param.name == 'b':
        param.update_rule.add_hook(GradientScaling(2))
    else:
        param.update_rule.add_hook(WeightDecay(0.0001))

updater = training.updaters.StandardUpdater(train_iter, optimizer, device=gpu_id)
trainer = training.Trainer(updater, (num_epochs, 'epoch'), 'detection_results')

log_interval = 100, 'iteration'
trainer.extend(DetectionVOCEvaluator(val_iter, model_detector, use_07_metric=False,label_names=eval_categories),trigger=log_interval)
trainer.extend(extensions.LogReport(trigger=log_interval))
trainer.extend(extensions.observe_lr(), trigger=log_interval)
trainer.extend(extensions.PrintReport(['epoch', 'iteration', 'lr','main/loss', 'main/loss/loc', 'main/loss/conf','validation/main/map','elapsed_time']), trigger=log_interval)
trainer.extend(extensions.snapshot_object(model_detector, 'model_epoch_{.updater.epoch}'), trigger=(1, 'epoch'))
trainer.run()

# pred
from chainercv.utils.bbox.non_maximum_suppression import non_maximum_suppression

def detect_tabacco(model, img_dir, fname, stride=300, crop_size=600):
    img = read_image(os.path.join(img_dir, fname))
    B = []
    L = []
    S = []
    for i in range(0, img.shape[1], stride):
        for j in range(0, img.shape[2], stride):
            img_cropped = img[:, i:i + crop_size, j:j + crop_size]
            bboxes, labels, scores = model.predict([img_cropped])
            B.append(bboxes[0] + [i, j, i, j])
            L.append(labels[0])
            S.append(scores[0])
    B = np.concatenate(B)
    L = np.concatenate(L)
    S = np.concatenate(S)
    r = non_maximum_suppression(bbox=B, thresh=0.1, score=S)

    return B[r], L[r], S[r]

chainer.serializers.load_npz(os.path.join('detection_results', 'model_epoch_{}'.format(20)), model_detector)

test_img_dir = 'test_images_1'
fname = 'test_015.jpg'
bboxes, labels, scores = detect_tabacco(model_detector, test_img_dir, fname)
img = read_image(os.path.join(test_img_dir, fname))
vis_bbox(img, bboxes, labels, scores, label_names=eval_categories)
plt.show()



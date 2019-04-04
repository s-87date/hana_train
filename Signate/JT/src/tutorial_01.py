import pandas as pd
import os
import json

dat_dir = '/Users/hanadate/work/hana_train/Signate/JT/dat/'
master = pd.read_csv(dat_dir+'master.tsv', sep='\t')

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

annotations = os.listdir(dat_dir+'train_annotations')
print(annotations)

category_data = {}
for annotation in annotations:
    with open(os.path.join(dat_dir+'train_annotations', annotation)) as f:
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
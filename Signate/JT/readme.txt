1. Data Format of the Annotation
・File Name: ***.json (*** = same as the corresponding image file name(*** in ***.jpg).)
・Description:
  - attributes:
      - mode: "train|test"
  - labels []:
      - category: "category_id"
      - box2d:
          - x1: int
          - y1: int
          - x2: int
          - y2; int
・Notes:
   - (x1, y1, x2, y2) corresponds to (left, top, right, bottom).

2. Submission File Format
・File Name: ***.json (*** = whatever name you like(e.g. submit))
・Description:
  - image_file_0:
      - category_1: [[x1, y1, x2, y2],...]
      - category_2: [[x1, y1, x2, y2],...]
      ...
  - image_file_1:
      - category_1: [[x1, y1, x2, y2],...]
      - category_2: [[x1, y1, x2, y2],...]
      ...
  ...
・Notes:
  - (x1, y1, x2, y2) corresponds to (left, top, right, bottom).
  - For each image file, the number of categories may differ.
  - Please also refer to "sample_submit.json".
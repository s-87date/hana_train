1. Data Format of the Annotation
�EFile Name: ***.json (*** = same as the corresponding image file name(*** in ***.jpg).)
�EDescription:
  - attributes:
      - mode: "train|test"
  - labels []:
      - category: "category_id"
      - box2d:
          - x1: int
          - y1: int
          - x2: int
          - y2; int
�ENotes:
   - (x1, y1, x2, y2) corresponds to (left, top, right, bottom).

2. Submission File Format
�EFile Name: ***.json (*** = whatever name you like(e.g. submit))
�EDescription:
  - image_file_0:
      - category_1: [[x1, y1, x2, y2],...]
      - category_2: [[x1, y1, x2, y2],...]
      ...
  - image_file_1:
      - category_1: [[x1, y1, x2, y2],...]
      - category_2: [[x1, y1, x2, y2],...]
      ...
  ...
�ENotes:
  - (x1, y1, x2, y2) corresponds to (left, top, right, bottom).
  - For each image file, the number of categories may differ.
  - Please also refer to "sample_submit.json".
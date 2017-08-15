# Distance Calculation
# ヒュベニの公式

#-------------------------------------------------
# latlon(A地東経、A地北緯、B地東経、B地北緯)　の順番で引数を渡す
# a: ax(経度), ay（緯度）
# b: bx(経度), by（緯度）
# Parameters:
pi = 3.1415926535 #円周率
p = 6378137 #地球半径
#-------------------------------------------------
latlon <- function(ax,ay, bx,by){
  xdif <- (ax - bx)*(pi/180) # x dif (経度の差)
  ydif <- (ay - by)*(pi/180) # y dif (緯度の差)
  deltax <- p*xdif*cos(ay*(pi/180)) # 経度の差に基づく東西距離(m)
  deltay <- p*ydif           # 緯度の差に基づく南北距離(m)
  sqrt(deltax^2 + deltay^2)  # Distance(m)
}

latlon(139.767044,35.680737,139.770654,35.691569)

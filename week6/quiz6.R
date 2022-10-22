
D = matrix(0, 4, 4)
D[2:4, 1] = c(0.3,0.4,0.7)
D[3:4, 2] = c(0.5,0.8)
D[4, 3]   = c(0.45)
D = D + t(D)

Dd = dist(D)
Dd

A_z = matrix(c(0.6, 0.4, 0.4, 0.6), 2, 2)
Pt1 = c(0.5, 0.5)

Pt2 = Pt1 %*% A_z
Pt3 = Pt2 %*% A_z



# install.packages('VennDiagram')
library(VennDiagram)

grid.newpage()
draw.triple.venn(area1 = 3, area2 = 200, area3 = 13, n12 = 2, n23 = 4, n13 = 2, 
    n123 = 1, category = c("Dog People", "Cat People", "Lizard People"), lty = "blank", 
    fill = c("skyblue", "pink1", "mediumorchid"), euler.d=T, scaled=T)

venn.diagram(list(B = 1:1800, A = 1571:2020),fill = c("red", "green"),
  alpha = c(0.5, 0.5), cex = 2,cat.fontface = 4,lty =2, fontfamily =3, filename='/Users/m.wehrens/Desktop/trial.emf')

# =====

# install.packages('venneuler')
library(venneuler)
v <- venneuler(c(A=450, B=1800, "A&B"=230))
plot(v)
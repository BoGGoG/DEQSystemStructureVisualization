# DEQSystemStructureVisualization
Mathematica package for visually inspecting the structure of a system of differential equations


Usage
----------------------------------------

Create a system of differential equations
```mathematica
deq1 = qf f''[x] + pf f'[x] + wf f[x] + s1 + qgx D[g[x,y], {x,2}] + qgy D[g[x,y], {y,2}]
deq2 = qf2 f''[x] + qgxy2 D[g[x,y], x,y] + wg2 g[x,y] + s2
deqs = {deq1, deq2};
```

The functions we want to track
```mathematica
fields = {f[x], g[x,y]}
```

```mathematica
ShowDEQSystemStructure[deqs, fields, "MaxDerivativeOrder"->2]
```

![Output screenshot](/screenshots/outputScreenshot.png)

Author
----------------------------------------
Marco Knipfer   
University of Alabama

# DivisionByZero

**Input state**:
- w: $⊤_{b}^{\text{\#}}$, x: [-1, 1], y: $⊤_{b}^{\text{\#}}$, z: $⊤_{b}^{\text{\#}}$

**Input interval boundaries**:
- m = -∞, n = +∞
```pascal
begin
  y = 7 / x;
  y = y + 1;
  z = x + y;
  while (y > -1) do -- (0)
    y = y - 1;
    w = w / y;
  done;
end
```
**Abstract loop invariants**:
- (0): w: $⊤_{b}^{\text{\#}}$, x: [-1, 1], y: [-6, 8], z: [-7, 9]

**Output state**:
- w: $⊤_{b}^{\text{\#}}$, x: [-1, 1], y: [-6, -1], z: [-7, 9]


**Runtime error alarms**:
- y: possible division by zero
- z: possible division by zero

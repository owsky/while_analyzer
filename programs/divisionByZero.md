# DivisionByZero

**Input state**:
- w: $⊤_{b}^{\text{\#}}$, x: [-1, 1], y: $⊤_{b}^{\text{\#}}$, z: $⊤_{b}^{\text{\#}}$

**Input interval boundaries**:
- m = -∞, n = +∞
```pascal
begin
  y = 7 / x; -- (0)
  y = y + 1; -- (1)
  z = x + y; -- (2)
  while (y > -1) do -- (3)
    y = y - 1; -- (4)
    w = w / y; -- (5)
  done;
end
```
**Abstract loop invariants**:
- (3): w: $⊤_{b}^{\text{\#}}$, x: [-1, 1], y: [-6, 8], z: [-7, 9]

**Output state**:
- w: $⊤_{b}^{\text{\#}}$, x: [-1, 1], y: [-6, -1], z: [-7, 9]


**Runtime error alarms**:
- (0): possible division by zero
- (5): possible division by zero

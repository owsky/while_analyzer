# DeepNesting

**Input state**:
- x: $⊤_{b}^{\text{\#}}$, y: $⊤_{b}^{\text{\#}}$, z: $⊤_{b}^{\text{\#}}$

**Input interval boundaries**:
- m = -∞, n = +∞
```pascal
begin
  x = 20; -- (0)
  y = -2; -- (1)
  z = x + y * 3 / -x * -y; -- (2)
  while (x - z <= 100000) do -- (3)
    z = z - 1; -- (4)
    while (y != 100) do -- (5)
      y = y + 1; -- (6)
    done;
  done;
  while (x + y + z != 9999) do -- (7)
    x = x + 1; -- (8)
    y = y + x; -- (9)
    z = z * 3; -- (10)
  done;
end
```
**Abstract loop invariants**:
- (3): x: [20, 20], y: [-2, 100], z: [-99981, -1]
- (5): x: [20, 20], y: [-2, +∞], z: [-99981, -2]
- (7): x: [20, +∞], y: [-2, +∞], z: [-∞, -99981]

**Output state**:
- x: [20, +∞], y: [-2, +∞], z: [-∞, -99981]


**Runtime error alarms**:
None
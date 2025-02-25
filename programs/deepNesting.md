# DeepNesting

**Input state**: x: $⊤_{b}^{\text{\#}}$, y: $⊤_{b}^{\text{\#}}$, z: $⊤_{b}^{\text{\#}}$
**Input interval boundaries**: m = -∞, n = +∞
```pascal
begin
  x = 20;
  y = -2;
  z = x + y * 3 / -x * -y;
  while (x - z <= 100000) do // (0)
    z = z - 1;
    while (y != 100) do // (1)
      y = y + 1;
    done;
  done;
  while (x + y + z != 9999) do // (2)
    x = x + 1;
    y = y + x;
    z = z * 3;
  done;
end
```
**Abstract loop invariants**:
- (0): x: [20, 20], y: [-2, 100], z: [-99981, -1]
- (1): x: [20, 20], y: [-2, +∞], z: [-99981, -2]
- (2): x: [20, +∞], y: [-2, +∞], z: [-∞, -99981]

**Output state**: x: [20, +∞], y: [-2, +∞], z: [-∞, -99981]

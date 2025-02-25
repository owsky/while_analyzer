# Constants

**Input state**: x: $⊤_{b}^{\text{\#}}$, y: $⊤_{b}^{\text{\#}}$
**Input interval boundaries**: m = -∞, n = +∞
```pascal
begin
  x = 0;
  y = 5;
  while (y < 20) do // (0)
    x = x + 3;
    y = y + x;
    x = x - 3;
  done;
end
```
**Abstract loop invariants**:
- (0): x: [0, 0], y: [5, 22]

**Output state**: x: [0, 0], y: [20, 22]

# NoInputState

**Input state**: x: $⊤_{b}^{\text{\#}}$, y: $⊤_{b}^{\text{\#}}$
**Input interval boundaries**: m = -∞, n = +∞
```pascal
begin
  x = 1;
  y = 10;
  while x < 1000 do
    x = y + 1;
  done;
end
```
**Output state**: x: [1000, 1000], y: [10, 10]
**Abstract loop invariants**:
_Loop guard_: $x < 1000$
_Loop invariant_: x: [1, 1000], y: [10, 10]

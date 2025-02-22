# Example

**Input state**: x: $⊤_{b}^{\text{\#}}$, y: [100, 100], z: $⊤_{b}^{\text{\#}}$
**Input interval boundaries**: m = -∞, n = +∞
```pascal
begin
  x = 7 / y;
  while x < y do
    x = x + 4;
    y = y + 1;
  done;
  if x == y then
    z = 3;
  else
    z = 1;
  endif;
end
```
**Output state**: x: [100, +∞], y: [100, +∞], z: [1, 3]
**Abstract loop invariants**:
_Loop guard_: $x < y$
_Loop invariant_: x: [0, +∞], y: [100, +∞], z: $⊤_{b}^{\text{\#}}$

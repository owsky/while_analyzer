# Example

**Input state**: x: $⊤_{b}^{\text{\#}}$, y: [100, 100], z: $⊤_{b}^{\text{\#}}$
**Input interval boundaries**: m = -∞, n = +∞
```pascal
begin
  x = 7 / 0;
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
**Output state**: $⊥^{\text{\#}}$
**Abstract loop invariants**:
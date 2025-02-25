# IfBottom

**Input state**: x: $⊤_{b}^{\text{\#}}$
**Input interval boundaries**: m = -∞, n = +∞
```pascal
begin
  if (false) then
    x = 1 / 0;
  else
    skip;
  endif;
end
```
**Abstract loop invariants**:

**Output state**: x: $⊤_{b}^{\text{\#}}$

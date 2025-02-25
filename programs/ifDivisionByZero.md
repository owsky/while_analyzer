# IfDivisionByZero

**Input state**: x: $⊤_{b}^{\text{\#}}$, y: $⊤_{b}^{\text{\#}}$
**Input interval boundaries**: m = -∞, n = +∞
```pascal
begin
  x = 0;
  if (1 / x == 2) then
    y = y + 5;
  else
    x = 1;
  endif;
end
```
**Abstract loop invariants**:

**Output state**: x: [0, 1], y: $⊤_{b}^{\text{\#}}$

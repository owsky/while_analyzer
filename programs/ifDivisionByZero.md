# IfDivisionByZero

**Input state**:
- x: $⊤_{b}^{\text{\#}}$, y: $⊤_{b}^{\text{\#}}$

**Input interval boundaries**:
- m = -∞, n = +∞
```pascal
begin
  if (1 / 0 == 2) then -- (0)
    y = y + 5; -- (1)
  else
    x = 1; -- (2)
  endif;
end
```
**Output state**:
- x: $⊤_{b}^{\text{\#}}$, y: $⊤_{b}^{\text{\#}}$


**Runtime error alarms**:
- (0): possible division by zero

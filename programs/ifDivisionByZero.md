# IfDivisionByZero

**Input state**:
- x: $⊤_{b}^{\text{\#}}$, y: $⊤_{b}^{\text{\#}}$

**Analysis input parameters**:
- Interval bounds m = -∞, n = +∞
- Widening Delay = 1
- Descending Steps = 2
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
- (0) Division by zero

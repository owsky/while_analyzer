# IfBottom

**Input state**:
- x: $⊤_{b}^{\text{\#}}$

**Analysis input parameters**:
- Interval bounds m = -∞, n = +∞
- Widening Delay = 1
- Descending Steps = 2
```pascal
begin
  if (true) then -- (0)
    x = 1 / 0; -- (1)
  else
    skip; -- (2)
  endif;
end
```
**Output state**:
- $⊥^{\text{\#}}$


**Runtime error alarms**:
- (1) Division by zero

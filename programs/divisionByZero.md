# DivisionByZero

**Input state**:
- w: $⊤_{b}^{\text{\#}}$, x: [-1, 1], y: $⊤_{b}^{\text{\#}}$

**Analysis input parameters**:
- Interval bounds m = -∞, n = +∞
- Widening Delay = 1
- Descending Steps = 2
```pascal
begin
  y = 7 / x; -- (0)
  y = y + 1; -- (1)
  while (y > -1) do -- (2)
    y = y - 1; -- (3)
    w = w / y; -- (4)
  done;
end
```
**Abstract loop invariants**:
- (2): w: $⊤_{b}^{\text{\#}}$, x: [-1, 1], y: [-6, 8]

**Output state**:
- w: $⊤_{b}^{\text{\#}}$, x: [-1, 1], y: [-6, -1]


**Runtime error alarms**:
- (0) Possible division by zero
- (4) Possible division by zero

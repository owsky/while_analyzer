# NoInputState

**Input state**:
- x: $⊤_{b}^{\text{\#}}$, y: $⊤_{b}^{\text{\#}}$

**Analysis input parameters**:
- Interval bounds m = -∞, n = +∞
- Widening Delay = 1
- Descending Steps = 2
```pascal
begin
  x = 1; -- (0)
  y = 10; -- (1)
  while (x < 11) do -- (2)
    x = y + 1; -- (3)
  done;
end
```
**Abstract loop invariants**:
- (2): x: [1, 11], y: [10, 10]

**Output state**:
- x: [11, 11], y: [10, 10]


**Runtime error alarms**:
None
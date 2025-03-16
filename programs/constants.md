# Constants

**Input state**:
- x: $⊤_{b}^{\text{\#}}$, y: $⊤_{b}^{\text{\#}}$

**Analysis input parameters**:
- Interval bounds m = -∞, n = +∞
- Widening Delay = 1
- Descending Steps = 2
```pascal
begin
  x = 0; -- (0)
  y = 5; -- (1)
  while (y < 20) do -- (2)
    x = x + 3; -- (3)
    y = y + x; -- (4)
    x = x - 3; -- (5)
  done;
end
```
**Abstract loop invariants**:
- (2): x: [0, 0], y: [5, 22]

**Output state**:
- x: [0, 0], y: [20, 22]


**Runtime error alarms**:
None
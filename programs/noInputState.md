# NoInputState

**Input state**:
- x: $⊤_{b}^{\text{\#}}$, y: $⊤_{b}^{\text{\#}}$

**Input interval boundaries**:
- m = -∞, n = +∞
```pascal
begin
  x = 1;
  y = 10;
  while (x < 11) do -- (0)
    x = y + 1;
  done;
end
```
**Abstract loop invariants**:
- (0): x: [1, 11], y: [10, 10]

**Output state**:
- x: [11, 11], y: [10, 10]


**Runtime error alarms**:
None
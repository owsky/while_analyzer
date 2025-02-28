# Example

**Input state**:
- x: $⊤_{b}^{\text{\#}}$, y: [-1, 1], z: $⊤_{b}^{\text{\#}}$

**Input interval boundaries**:
- m = -∞, n = +∞
```pascal
begin
  x = 7 / y;
  while (x < y) do -- (0)
    x = x + 4;
    y = y + 1;
  done;
  if (x == y) then
    z = 3;
  else
    z = 1;
  endif;
end
```
**Abstract loop invariants**:
- (0): x: [-7, +∞], y: [-1, +∞], z: $⊤_{b}^{\text{\#}}$

**Output state**:
- x: [-1, +∞], y: [-1, +∞], z: [1, 3]


**Runtime error alarms**:
- x: possible division by zero

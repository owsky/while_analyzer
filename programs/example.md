# Example

**Input state**:
- x: $⊤_{b}^{\text{\#}}$, y: [-1, 1], z: $⊤_{b}^{\text{\#}}$

**Input interval boundaries**:
- m = -∞, n = +∞
```pascal
begin
  x = 7 / y; -- (0)
  while (x < y) do -- (1)
    x = x + 4; -- (2)
    y = y + 1; -- (3)
  done;
  if (x == y) then -- (4)
    z = 3; -- (5)
  else
    z = 1; -- (6)
  endif;
end
```
**Abstract loop invariants**:
- (1): x: [-7, +∞], y: [-1, +∞], z: $⊤_{b}^{\text{\#}}$

**Output state**:
- x: [-1, +∞], y: [-1, +∞], z: [1, 3]


**Runtime error alarms**:
- (0): possible division by zero

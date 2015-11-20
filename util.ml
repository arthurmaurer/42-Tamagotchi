
let clamp value min max =
	if value < min then min
	else if value > max then max
	else value

let applyPercentage value total =
	int_of_float (((float_of_int total) *. (float_of_int value)) /. 100.0)

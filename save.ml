
let save (character:Character.character) =
	if character#isAlive () then
	begin
		try
			let fd = open_out_gen [Open_wronly; Open_creat] 0o755 "save.itama" in
				output_string fd character#toString;
				close_out fd;
		with
			| _ -> ()
	end
	else
	begin
		try
			Sys.remove "save.itama"
		with
			| _ -> ()
	end

let load () =
	try
		let fd = open_in "save.itama" in
		let dt = ((int_of_float (Unix.time ())) - (int_of_string (input_line fd)) )in
		let rec createArray n acc =
			if n > 0 then
			begin
				try
					let line = input_line fd in
					try
					let v =	match n with
					| 5 -> begin
						if ((int_of_string line) - dt) > 0 then
						((int_of_string line) - dt)
						else
						0
						end
					| 1 -> ((int_of_string line) + dt)
					| _ -> (int_of_string line)
				in
					createArray (n - 1) (Array.append acc [|v|])
					with
						|_ -> [| 100; 100; 100; 100; 0 |];
				with
					|_ -> [| 100; 100; 100; 100; 0 |];
			end
			else acc
		in
		createArray 5 (Array.make 0 1);
	with
		|_ -> [| 100; 100; 100; 100; 0 |];

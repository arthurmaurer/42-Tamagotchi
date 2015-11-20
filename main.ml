let moveCursor (parameters:Tama.parameters) dir = match dir with
| 0 -> let action =
		match parameters.action with
			| Tama.Eat -> parameters.action
			| Tama.Thunder -> Tama.Eat
			| Tama.Bath -> Tama.Thunder
			| Tama.Kill -> Tama.Bath
			| Tama.Sleep -> Tama.Kill
			| Tama.Dance -> Tama.Sleep
			| Tama.Dictate -> Tama.Dance
			| Tama.Twerk -> Tama.Dictate
	in
	{ parameters with action = action }
| 1 -> let action =
		match parameters.action with
			| Tama.Eat -> Tama.Thunder
			| Tama.Thunder -> Tama.Bath
			| Tama.Bath -> Tama.Kill
			| Tama.Kill -> Tama.Sleep
			| Tama.Sleep -> Tama.Dance
			| Tama.Dance -> Tama.Dictate
			| Tama.Dictate -> Tama.Twerk
			| Tama.Twerk -> parameters.action
	in
	{ parameters with action = action }
| _ -> parameters

let doAction (parameters:Tama.parameters) = match parameters.action with
	| Tama.Eat -> { parameters with
		character = (parameters.character#eat ());
		animation = { parameters.animation with count = 0; current = Tama.Eat }
	}
	| Tama.Thunder -> { parameters with
		character = (parameters.character#thunder ());
		animation = { parameters.animation with count = 0; current = Tama.Thunder }
	}
	| Tama.Bath -> { parameters with
		character = (parameters.character#bath ());
		animation = { parameters.animation with count = 0; current = Tama.Bath }
	}
	| Tama.Kill -> { parameters with
		character = (parameters.character#kill ());
		animation = { parameters.animation with count = 0; current = Tama.Kill }
	}
	| Tama.Sleep -> { parameters with
		character = (parameters.character#sleep ());
		animation = { parameters.animation with count = 0; current = Tama.Sleep }
	}
	| Tama.Dance -> { parameters with
		character = (parameters.character#dance ());
		animation = { parameters.animation with count = 0; current = Tama.Dance }
	}
	| Tama.Dictate -> { parameters with
		character = (parameters.character#dictate ());
		animation = { parameters.animation with count = 0; current = Tama.Dictate }
	}
	| Tama.Twerk -> { parameters with
		character = (parameters.character#twerk ());
		animation = { parameters.animation with count = 0; current = Tama.Twerk }
	}

let handleEvents (parameters:Tama.parameters) =
	match Sdlevent.poll () with
		| None -> { parameters with run = true }
		| Some event -> match event with
			| Sdlevent.KEYDOWN { keysym = Sdlkey.KEY_ESCAPE } ->
				{ parameters with run = false }
			| Sdlevent.KEYDOWN { keysym = Sdlkey.KEY_LEFT } ->
				moveCursor parameters 0
			| Sdlevent.KEYDOWN { keysym = Sdlkey.KEY_RIGHT } ->
				moveCursor parameters 1
			| Sdlevent.KEYDOWN { keysym = Sdlkey.KEY_SPACE } ->
				doAction parameters
			| event -> { parameters with run = true }

let getAnim (anim:Tama.animationData) count =
	if anim.current = Tama.Dance && count > 4 ||
		anim.current = Tama.Kill && count > 4 ||
		anim.current = Tama.Dictate && count > 2 ||
		anim.current = Tama.Twerk && count > 3 ||
		anim.current = Tama.Sleep && count > 2 ||
		anim.current = Tama.Thunder && count > 2 ||
		anim.current = Tama.Bath && count > 2 ||
		anim.current = Tama.Eat && count > 2 ||
		anim.current = Tama.Yawn && count > 1
	then
		{ anim with current = Tama.None; count = 0 }
	else if anim.current = Tama.None then
		match Random.int 800 with
			| 0 -> { anim with count = 0; current = Tama.Yawn }
			| _ -> { anim with count = count }
	else
		{ anim with count = count }

let rec update window (parameters:Tama.parameters) =
	let t = Unix.time () in
    let dt = (t -. parameters.lasttime) in
	let p = handleEvents parameters in
	let newParam =
		if dt >= 1.0 then
			{ parameters with character = (p.character#update ()) }
		else
			p
	in

	let animCount = if dt >= 1.0 then p.animation.count + 1 else p.animation.count in

	window#render newParam;

	if newParam.run then
		update window { newParam with lasttime = t; animation = getAnim newParam.animation animCount }
	else
		Save.save newParam.character

let () =
	Random.self_init ();
	Window.init ();
	let savedData = Save.load () in
	let character = new Character.character savedData.(0) savedData.(1) savedData.(2) savedData.(3) savedData.(4) in
	let window = new Window.window in

	update window {
		run = true;
		character = character;
		action = Tama.Eat;
		lasttime = (Unix.time ());
		animation = {
			current = Tama.None;
			count = 0
		}
	}

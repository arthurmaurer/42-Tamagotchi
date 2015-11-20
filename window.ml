
let windowSize = ( 400, 400 )

let init () = 
	Sdl.init [`VIDEO];
	at_exit Sdl.quit;
	Sdlttf.init ();
	at_exit Sdlttf.quit

class window =
	object (self)

		val _screen = Sdlvideo.set_video_mode (fst windowSize) (snd windowSize) [`DOUBLEBUF]
		val _font = Sdlttf.open_font "resources/font.ttf" 14
		val _fontLarge = Sdlttf.open_font "resources/font.ttf" 40
		val _sprites = Sdlloader.load_image "resources/sprites.png"
		val _spriteSize = 160
		val _spritePadding = 40

		method private clear () =
			let rect = Sdlvideo.rect 0 0 (fst windowSize) (snd windowSize) in
			let color = Int32.of_int 0xeeeeeeff in

			Sdlvideo.fill_rect ~rect:rect _screen color;

		method private renderStatusBar (label:string) (x:int) (y:int) (percentage:int) =
			let width = 85 in
			let height = 20 in
			let margin = 3 in
			let innerWidth = Util.applyPercentage (100 - percentage) (width - margin * 2) in

			let outerRect = Sdlvideo.rect x (y + 22) width height in
			let innerRect = Sdlvideo.rect (x - margin + width - innerWidth) (y + 22 + margin) (innerWidth) (height - margin * 2) in

			let outerColor = Int32.of_int 0x000000ff in
			let innerColor = Int32.of_int 0xffffffff in

			let text = Sdlttf.render_text_blended _font label ~fg:Sdlvideo.black in
			let textPosition = Sdlvideo.rect x y 300 300 in

			Sdlvideo.blit_surface ~dst_rect:textPosition ~src:text ~dst:_screen ();
			Sdlvideo.fill_rect ~rect:outerRect _screen outerColor;
			Sdlvideo.fill_rect ~rect:innerRect _screen innerColor

		method private renderButton (label:string) (x:int) (y:int) (active:bool) =
			let width = 85 in
			let height = 40 in
			let margin = 3 in

			let outerRect = Sdlvideo.rect x y width height in
			let innerRect = Sdlvideo.rect (x + margin) (y + margin) (width - margin * 2) (height - margin * 2) in

			let outerColor = Int32.of_int 0x000000ff in
			let innerColor = Int32.of_int (if active then 0x000000ff else 0xffffffff) in

			let textColor = (if active then Sdlvideo.white else Sdlvideo.black) in
			let text = Sdlttf.render_text_blended _font label ~fg:textColor in
			let textWidth = fst (Sdlttf.size_text _font label) in
			let textPosition = Sdlvideo.rect (x + width / 2 - textWidth / 2) (y + height / 2 - 11) 300 300 in

			Sdlvideo.fill_rect ~rect:outerRect _screen outerColor;
			Sdlvideo.fill_rect ~rect:innerRect _screen innerColor;
			Sdlvideo.blit_surface ~dst_rect:textPosition ~src:text ~dst:_screen ()

		method private renderStatusBars (character:Character.character) =
			self#renderStatusBar "HEALTH" 15 5 (character#getHealth ());
			self#renderStatusBar "ENERGY" 110 5 (character#getEnergy ());
			self#renderStatusBar "HYGIENE" 205 5 (character#getHygiene ());
			self#renderStatusBar "HAPPINESS" 300 5 (character#getHappiness ())

		method private renderButtons (action:Tama.action) =
			self#renderButton "EAT" 15 300 (action = Tama.Eat);
			self#renderButton "THUNDER" 110 300 (action = Tama.Thunder);
			self#renderButton "BATH" 205 300 (action = Tama.Bath);
			self#renderButton "KILL" 300 300 (action = Tama.Kill);
			self#renderButton "SLEEP" 15 350 (action = Tama.Sleep);
			self#renderButton "DANCE" 110 350 (action = Tama.Dance);
			self#renderButton "DICTATE" 205 350 (action = Tama.Dictate);
			self#renderButton "TWERK" 300 350 (action = Tama.Twerk)

		method private getSpriteRect (status:Character.status) =
			let position = match status with
				| Neutral -> ( 2, 4 )
				| Happy -> ( 4, 10 )
				| Dirty -> ( 3, 9 )
				| Sad -> ( 8, 29 )
				| Tired -> ( 3, 4 )
				| Weak -> ( 2, 15 )
				| Dead -> ( 3, 23 )
				| Dance1 -> ( 5, 6 )
				| Dance2 -> ( 5, 5 )
				| Dance3 -> ( 5, 6 )
				| Dance4 -> ( 5, 3 )
				| Sleep1 -> ( 4, 25 )
				| Sleep2 -> ( 4, 22 )
				| Dictate1 -> ( 1, 5 )
				| Dictate2 -> ( 1, 6 )
				| Twerk1 -> ( 5, 28 )
				| Twerk2 -> ( 5, 27 )
				| Eat1 -> ( 1, 19 )
				| Eat2 -> ( 1, 20 )
				| Thunder1 -> ( 2, 19 )
				| Thunder2 -> ( 2, 20 )
				| Bath1 -> ( 3, 24 )
				| Bath2 -> ( 3, 8 )
				| Kill1 -> ( 7, 5 )
				| Kill2 -> ( 7, 6 )
				| Kill3 -> ( 7, 5 )
				| Kill4 -> ( 7, 8 )
				| Yawn1 -> ( 3, 5 )
				| Yawn2 -> ( 3, 3 )
			in
			Sdlvideo.rect ((_spriteSize + _spritePadding) * (fst position)) ((_spriteSize + _spritePadding) * (snd position)) _spriteSize _spriteSize

		method private getCharacterStatus (parameters:Tama.parameters) =
			let character = parameters.character in

			if character#isAlive () = false then
				character#getStatus ()
			else
			begin
				match parameters.animation.current with
					| None -> character#getStatus ()
					| Dance -> begin
						match parameters.animation.count mod 4 with
							| 0 -> Dance1
							| 1 -> Dance2
							| 2 -> Dance3
							| 3 -> Dance4
					end
					| Sleep -> begin
						match parameters.animation.count mod 2 with
							| 0 -> Sleep1
							| 1 -> Sleep2
					end
					| Dictate -> begin
						match parameters.animation.count mod 2 with
							| 0 -> Dictate1
							| 1 -> Dictate2
					end
					| Twerk -> begin
						match parameters.animation.count mod 2 with
							| 0 -> Twerk1
							| 1 -> Twerk2
					end
					| Eat -> begin
						match parameters.animation.count mod 2 with
							| 0 -> Eat1
							| 1 -> Eat2
					end
					| Kill -> begin
						match parameters.animation.count mod 4 with
							| 0 -> Kill1
							| 1 -> Kill2
							| 2 -> Kill3
							| 3 -> Kill4
					end
					| Yawn -> begin
						match parameters.animation.count mod 3 with
							| 0 -> Yawn1
							| 1 -> Yawn2
							| 2 -> Yawn3
					end
					| Bath -> begin
						match parameters.animation.count mod 2 with
							| 0 -> Bath1
							| 1 -> Bath2
					end
					| Thunder -> begin
						match parameters.animation.count mod 2 with
							| 0 -> Thunder1
							| 1 -> Thunder2
					end
			end

		method private renderCharacter (parameters:Tama.parameters) =
			let status = self#getCharacterStatus parameters in
			let srcRect = self#getSpriteRect status in
			let dstRect = Sdlvideo.rect ((fst windowSize) / 2 - (_spriteSize / 2)) 100 0 0 in

			Sdlvideo.blit_surface ~src_rect:srcRect ~dst_rect:dstRect ~src:_sprites ~dst:_screen ();
			self#renderAge parameters.character

		method private renderDeathSubText (character:Character.character) =
			let str =
				if character#getHealth () <= 0 then "Starved to death."
				else if character#getEnergy () <= 0 then "Too soon to be OOM"
				else if character#getHygiene () <= 0 then "The autopsy revealed the presence of poop in the lungs."
				else if character#getHappiness () <= 0 then "This life was too boring for him."
				else ""
			in
			let textSize = Sdlttf.size_text _font str in
			let text = Sdlttf.render_text_blended _font str ~fg:Sdlvideo.black in
			let textPosition = Sdlvideo.rect
				((fst windowSize) / 2 - ((fst textSize) / 2))
				300 0 0
			in

			Sdlvideo.blit_surface ~dst_rect:textPosition ~src:text ~dst:_screen ()

		method private renderDeathText (character:Character.character) =
			let str = "DEAD" in
			let textSize = Sdlttf.size_text _fontLarge str in
			let text = Sdlttf.render_text_blended _fontLarge str ~fg:Sdlvideo.black in
			let textPosition = Sdlvideo.rect
				((fst windowSize) / 2 - ((fst textSize) / 2))
				30 0 0
			in

			Sdlvideo.blit_surface ~dst_rect:textPosition ~src:text ~dst:_screen ();
			self#renderDeathSubText character

		method private renderAge (character:Character.character) =
			let str = string_of_int (character#getAge ()) ^ " hours old" in
			let textSize = Sdlttf.size_text _font str in
			let text = Sdlttf.render_text_blended _font str ~fg:Sdlvideo.black in
			let textPosition = Sdlvideo.rect
				((fst windowSize) / 2 - ((fst textSize) / 2))
				250 0 0
			in

			Sdlvideo.blit_surface ~dst_rect:textPosition ~src:text ~dst:_screen ();

		method render (parameters:Tama.parameters) =
			self#clear ();

			if parameters.character#isAlive () then
				begin
					self#renderButtons parameters.action;
					self#renderStatusBars parameters.character;
				end
			else
				self#renderDeathText parameters.character;

			self#renderCharacter parameters;

			Sdlvideo.flip _screen
	end

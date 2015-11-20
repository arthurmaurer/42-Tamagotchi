
type status = Neutral | Happy | Sad |
	Tired | Weak | Dead | Dirty |
	Dance1 | Dance2 | Dance3 | Dance4 |
	Sleep1 | Sleep2 |
	Dictate1 | Dictate2 |
	Twerk1 | Twerk2 |
	Eat1 | Eat2 |
	Kill1 | Kill2 | Kill3 | Kill4 |
	Bath1 | Bath2 |
	Thunder1 | Thunder2 |
	Yawn1 | Yawn2 | Yawn3 | Yawn4

class character health energy hygiene happiness age =
	object (self)

		val _health = health
		val _energy = energy
		val _hygiene = hygiene
		val _happiness = happiness
		val _age = age
		val _status =
			if health <= 0 || energy <= 0 || hygiene <= 0 || happiness <= 0 then Dead
			else if health < 30 then Weak
			else if energy < 30 then Tired
			else if hygiene < 30 then Dirty
			else if happiness > 50 then Happy
			else if happiness < 50 then Sad
			else Neutral

		method getHealth () = _health
		method getEnergy () = _energy
		method getHygiene () = _hygiene
		method getHappiness () = _happiness
		method getStatus () = _status
		method getAge () = _age

		method setHealth x = (Util.clamp (_health + x) 0 100)
		method setEnergy x = (Util.clamp (_energy + x) 0 100)
		method setHygiene x = (Util.clamp (_hygiene + x) 0 100)
		method setHappiness x = (Util.clamp (_happiness + x) 0 100)

		method isAlive () = _health > 0 && _energy > 0 && _hygiene > 0 && _happiness > 0

		method clone () = new character
			(self#setHealth 0)
			(self#setEnergy 0)
			(self#setHygiene 0)
			(self#setHappiness 0)
			_age

		method toString = Printf.sprintf "%i\n%i\n%i\n%i\n%i\n%i" (int_of_float(Unix.time ())) _health _energy _hygiene _happiness _age

		method update () =
			if self#isAlive () then
				new character
				(self#setHealth (-1))
				(self#setEnergy 0)
				(self#setHygiene 0)
				(self#setHappiness 0)
				(_age + 1)
			else
				self#clone ()

		method eat () =
			if self#isAlive () then
				new character
				(self#setHealth 25)
				(self#setEnergy (-10))
				(self#setHygiene (-20))
				(self#setHappiness 5)
				_age
			else
				self#clone ()

		method twerk () =
			if self#isAlive () then
				new character
				(self#setHealth 0)
				(self#setEnergy (-10))
				(self#setHygiene (-5))
				(self#setHappiness 15)
				_age
			else
				self#clone ()

		method dictate () =
			if self#isAlive () then
				new character
				(self#setHealth 50)
				(self#setEnergy 50)
				(self#setHygiene 50)
				(self#setHappiness 50)
				_age
			else
				self#clone ()

		method thunder () =
			if self#isAlive () then
				new character
				(self#setHealth (-20))
				(self#setEnergy 25)
				(self#setHygiene 0)
				(self#setHappiness (-20))
				_age
			else
				self#clone ()

		method bath () =
			if self#isAlive () then
				new character
				(self#setHealth (-20))
				(self#setEnergy (-10))
				(self#setHygiene 25)
				(self#setHappiness 5)
				_age
			else
				self#clone ()

		method kill () =
			if self#isAlive () then
				new character
				(self#setHealth (-20))
				(self#setEnergy (-10))
				(self#setHygiene 0)
				(self#setHappiness 20)
				_age
			else
				self#clone ()

		method sleep () =
			if self#isAlive () then
				new character
				(self#setHealth (-10))
				(self#setEnergy 25)
				(self#setHygiene 0)
				(self#setHappiness 20)
				_age
			else
				self#clone ()

		method dance () =
			if self#isAlive () then
				new character
				(self#setHealth (-20))
				(self#setEnergy (-10))
				(self#setHygiene (-10))
				(self#setHappiness 20)
				_age
			else
				self#clone ()
	end

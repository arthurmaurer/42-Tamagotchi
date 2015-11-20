
type action = Eat | Thunder | Bath | Kill | Sleep | Dance | Dictate | Twerk | Yawn | None

type animationData = {
	current: action;
	count: int
}

type parameters = {
	run: bool;
	character: Character.character;
	action: action;
	lasttime: float;
	animation: animationData
}

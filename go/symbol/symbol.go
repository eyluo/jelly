package symbol

type T string

func New(s string) T {
	return T(s)
}

func (sym T) ToString() string {
	return string(sym)
}

package symbol

type T string

func (sym T) ToString() string {
	return string(sym)
}

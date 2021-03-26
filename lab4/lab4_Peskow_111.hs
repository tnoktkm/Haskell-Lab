{-(7). Теоретически возможно, хотя и неэффективно, определить целые
числа с помощью рекурсивных типов данных следующим образом:
data Number = Zero | Next Number
Т. е. число является либо нулем (Zero), либо определяется, как
число, следующее за предыдущим числом. Например, число 3 записывается как
Next (Next (Next Zero)). Определите для такого представления следующие функции:-}

data Number = Zero 
			| Next Number deriving (Show)

{-(1) fromInt, для заданного целого числа типа Integer
возвращающую соответствующее ему значение типа Number.-}

fromInt :: Integer -> Number 
fromInt 0 = Zero
fromInt integerNumber
				|integerNumber >= 0 = Next (fromInt (integerNumber - 1))
				|otherwise = error ("Error: expected arg >= 0")

{-(2) toInt, преобразующую значение типа Number в соответствующее целое число.-}

caughtFirstKindOfNumber :: Number -> Number
caughtFirstKindOfNumber (Next n) = n
caughtFirstKindOfNumber _ = Zero 

toInt :: Number -> Integer
toInt Zero = 0
toInt numericKindOfNumber = 1 + toInt (caughtFirstKindOfNumber numericKindOfNumber) 

{-(3) plus :: Number -> Number -> Number, складывающую
свои аргументы.-}

plus :: Number -> Number -> Number
plus numOfNumber1 numOfNumber2 = fromInt (toInt numOfNumber1 + toInt numOfNumber2)

{-(4) mult :: Number -> Number -> Number, умножающую
свои аргументы.-}

mult :: Number -> Number -> Number
mult numOfNumber1 numOfNumber2
				|num >= 0 = fromInt num
				|otherwise = error ("Error: expected arg >= 0") where num = toInt numOfNumber1 * toInt numOfNumber2

{-(5) dec, вычитающую единицу из своего аргумента. Для Zero
функция должна возвращать Zero.-}

dec :: Number -> Number
dec Zero = Zero
dec number =  fromInt (toInt number - 1)

{-(6) fact, вычисляющую факториал.-}

factorial :: Integer -> Integer
factorial 0 = 1
factorial n 
			|n > 0 = n * factorial (n - 1)
			|otherwise = error ("Error: expected arg >= 0")

factorialNumber :: Number -> Number
factorialNumber number = fromInt (factorial (toInt number)) 
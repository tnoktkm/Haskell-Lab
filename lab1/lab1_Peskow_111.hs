--1) Функция max3, по трем целым возвращающая наибольшее из них
max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = max a (max b c)		--находим из 2-х макс и сарвниваем его с 3-м

--2) Функция min3, по трем целым возвращающая наименьшее из них.
min3 :: Integer -> Integer -> Integer -> Integer
min3 a b c = min a (min b c)			--аналогично заданию 1

{-3) Функция sort2, по двум целым возвращающая пару, в которой
наименьшее из них стоит на первом месте, а наибольшее — на
втором. -}
sort2 :: Integer -> Integer -> (Integer, Integer)
sort2 a b = (min a b, max a b)			-- возвращаем пару макс из 2-х чисел, и мин из 2-х чисел

{-4) Функция bothTrue :: Bool -> Bool -> Bool, которая
возвращает True тогда и только тогда, когда оба ее аргумента
будут равны True. Не используйте при определении функции
стандартные логический операции (&&, || и т.п.).-}
bothTrue :: Bool -> Bool -> Bool			--возвращаем true только при True True все другое False
bothTrue True True = True
bothTrue _ _ = False

{-5) Функция solve2::Double->Double->(Bool,Double),
которая по двум числам, представляющим собой коэффициенты
линейного уравнения ax + b = 0, возвращает пару, первый
элемент которой равен True, если решение существует и False
в противном случае; при этом второй элемент равен либо
значению корня, либо 0.0.-}
solve2 :: Double -> Double -> (Bool, Double)
solve2 a b = if a == 0 then (False, 0.0) else (True, (-b)/a) -- стандартная формула нахождения корней линейного уравнения

{-6) Функция isParallel, возвращающая True, если два отрезка, концы которых задаются в аргументах функции, параллельны (или лежат на одной прямой). Например, значение выражения isParallel (1,1) (2,2) (2,0) (4,2) должно быть
равно True, поскольку отрезки (1, 1) − (2, 2) и (2, 0) − (4, 2) параллельны-}
isParallel :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isParallel (x1,y1) (x2,y2) (x3,y3) (x4,y4) = (x1-x2)*(y3-y4) == (y1-y2)*(x3-x4) --проверка на параллельность

{-7) Функция isIncluded, аргументами которой служат параметры
двух окружностей на плоскости (координаты центров и радиусы);
функция возвращает True, если вторая окружность целиком содержится внутри первой.-}
isIncluded :: (Double, Double, Double) -> (Double, Double, Double) -> Bool
isIncluded (x1, y1, r1) (x2, y2, r2) = sqrt((x2 - x1)^2 + (y2 - y1)^2) <= (r1 - r2) --формкла проверка окружностей

{-8) Функция isRectangular, принимающая в качестве параметров координаты трех точек на плоскости, и возвращающая True,
если образуемый ими треугольник — прямоугольный.-}
isRectangular :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isRectangular (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (x3 - x2) + (y2 - y1) * (y3 - y2) == 0 
||(x2-x1)*(x3-x1)+(y2-y1)*(y3-y1) == 0 
||(x2-x3)*(x1-x3)+(y2-y3)*(y1-y3) == 0  --обычная проверка на прямоугольность треугольника по координатам вершин

{-9) Функция isTriangle, определяющая, можно ли их отрезков с
заданными длинами x, y и z построить треугольник.-}
isTriangle :: Double -> Double -> Double -> Bool
isTriangle a b c = (c < a + b) && (b < a + c) && (a < b + c) --главное чтобы сумма 2-х сторон было БОЛЬШЕ третьей

{-10) Функция isSorted, принимающая на вход три числа и возвращающая True, если они упорядочены по возрастанию или по
убыванию.-}
isSorted :: Integer -> Integer -> Integer -> Bool
isSorted a b c = (a <= b && b <= c) || (a >= b && b >= c)	--просто проверяем условие
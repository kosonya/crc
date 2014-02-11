import Graphics.UI.Gtk

xor 0 1 = 1
xor 1 0 = 1
xor _ _ = 0

delete_zeros [] = []
delete_zeros (0:xs) = delete_zeros xs
delete_zeros xs = xs

xor_array = zipWith (\x y -> x `xor` y)

divide_step divident divisor = (map (\(x, y) -> x `xor` y) f') ++ l
	where
	d = delete_zeros divident
	len = length divisor
	f = take len d
	l = drop len d
	f' = zip f divisor

divide divident divisor
	| (length (delete_zeros divident)) < (length divisor) = divident
	| (length (delete_zeros divident)) == (length divisor) = delete_zeros(divide_step divident divisor)
	| otherwise = delete_zeros(divide (divide_step divident divisor) divisor)

zeros = (0:zeros)

shiftl xs n = xs ++ (take n zeros)

encode inf pol = inf ++ crc
	where
	clen = (length pol) - 1
	c = divide (shiftl inf clen) pol
	l = length c
	crc = (take (clen - l) zeros) ++ c

rotate [] _ = []
rotate l 0 = l
rotate (x:xs) (n+1) = rotate (xs ++ [x]) n
rotate l n = rotate l (length l + n)

calcones [] = 0
calcones (0:xs) = calcones xs
calcones (1:xs) = 1 + calcones xs

fillto xs n = take (n - (length(xs))) zeros ++ xs

decode' 31 _ _ = error "Can't correct error"
decode' sht code pol
	| (calcones res) <= 1 = code `xor_array` (res `fillto` l) `rotate` (negate sht)
	| otherwise = decode' (sht+1) (code `rotate` 1) pol
	where
		res = code `divide` pol
		l = length code
decode = decode' 0

makeError [] _ = []
makeError (1:xs) 1 = (0:xs)
makeError (0:xs) 1 = (1:xs)
makeError (x:xs) (n+1) = (x:(makeError xs n))

numToNumPol num
	| l <= 1 = (fillto num 1, [1,1,1])
	| l <= 4 = (fillto num 4, [1,0,1,1])
	| l <= 11 = (fillto num 11, [1,1,0,0,1])
	| l <= 26 = (fillto num 26, [1,0,1,0,0,1])
	| otherwise = ([], [])
	where l = length num

main :: IO ()
main= do
	initGUI
	window <- windowNew
	set window [windowTitle := "Циклический код", containerBorderWidth := 10, windowDefaultWidth := 250, windowDefaultHeight := 100]

	mainbox <- vBoxNew False 0
	containerAdd window mainbox

	encinpbox <- hBoxNew True 0
	boxPackStart mainbox encinpbox PackNatural 0

	inflabel <- labelNew Nothing
	boxPackStart encinpbox inflabel PackNatural 0
	labelSetText inflabel "До 26 информационных разрядов"
	inffield <- entryNew
	boxPackStart encinpbox inffield PackNatural 0
	
	encbutton <- buttonNewWithLabel "Закодировать"
	boxPackStart encinpbox encbutton PackNatural 0

	encresbox <- hBoxNew False 0
	boxPackStart mainbox encresbox PackNatural 0
	encresname <- labelNew Nothing
	boxPackStart encresbox encresname PackNatural 0
	labelSetText encresname "Кодовая комбинация:"
	encreslabel <- labelNew Nothing
	boxPackStart encresbox encreslabel PackNatural 0

	encpolbox <- hBoxNew False 0
	boxPackStart mainbox encpolbox PackNatural 0
	encpolname <- labelNew Nothing
	boxPackStart encpolbox encpolname PackNatural 0
	labelSetText encpolname "Выбран полином:"
	encpollabel <- labelNew Nothing
	boxPackStart encpolbox encpollabel PackNatural 0

	errorbox <- hBoxNew True 0
	boxPackStart mainbox errorbox PackNatural 0
	errorlabel <- labelNew Nothing
	boxPackStart errorbox errorlabel PackNatural  0
	labelSetText errorlabel "Выберите разряд, в котором сделать ошибку"
	errorspinbutton <- spinButtonNewWithRange 1 7 1
	boxPackStart errorbox errorspinbutton PackNatural 10
	errorbutton <- buttonNewWithLabel "Сделать ошибку"
	boxPackStart errorbox errorbutton PackNatural 10

	errresbox <- hBoxNew False 0
	boxPackStart mainbox errresbox PackNatural 0
	errreslabel <- labelNew Nothing
	boxPackStart errresbox errreslabel PackNatural 0
	labelSetText errreslabel "Кодовая комбинация с ошибкой:"
	errres <- labelNew Nothing
	boxPackStart errresbox errres PackNatural 0

	decodebox <- hBoxNew True 0
	boxPackStart mainbox decodebox PackNatural 0
	decodelabel <- labelNew Nothing
	boxPackStart decodebox decodelabel PackNatural 0
	labelSetText decodelabel "Кодовая комбинация после декодирования:"
	decoderes <- labelNew Nothing
	boxPackStart decodebox decoderes PackNatural 0
	decodebutton <- buttonNewWithLabel "Декодировать"
	boxPackStart decodebox decodebutton PackNatural 0


	widgetShowAll window
	onPressed encbutton (guiEncode inffield encreslabel encpollabel errorspinbutton)
	onPressed errorbutton (guiMakeError encreslabel errorspinbutton errres)
	onPressed decodebutton (guiDecode errres encpollabel decoderes)
	onDestroy window mainQuit
	mainGUI

readInf :: String ->[Int]
readInf str
	| foldl (\x y -> ((y /= 0) && (y /= 1)) || x) False res = []
	| otherwise = res
	where res = map (\x -> read [x]) str 

showPol = map  (\x -> (show x)!!0) 

guiEncode :: Entry -> Label -> Label -> SpinButton -> IO()
guiEncode inff resf polf spinb = do	
	inf1' <- entryGetText inff
	inf1 <- (return (readInf $ inf1'))
	(inf, pol) <- return (numToNumPol inf1)
	if inf1 == [] then do
			errorDialog "Введите информационную часть"
		else do
			let result = encode inf pol
			let l = fromIntegral (length result)
			labelSetText resf (showPol result)
			labelSetText polf (showPol pol)
			spinButtonSetRange spinb 1.0 l

errorDialog :: String -> IO ()
errorDialog errormsg = do
	dialog <- messageDialogNew Nothing [DialogModal] MessageError ButtonsOk errormsg
	rid <- dialogRun dialog
	widgetDestroy dialog
	return ()

guiMakeError::Label -> SpinButton -> Label -> IO()
guiMakeError codel errspin resl = do
	code' <- labelGetText codel
	code <- return (readInf $ code')
	err' <- spinButtonGetValueAsInt errspin
	err <- return (err')
	if code == [] then do
			errorDialog "Сначала надо закодировать!"
		else do
			labelSetText resl  (showPol (makeError code err))

guiDecode::Label -> Label -> Label -> IO()
guiDecode codel polb resl = do
	code' <- labelGetText codel
	code <- return (readInf $ code')
	pol' <- labelGetText polb
	pol <- return (readInf $ pol')
	if code == [] then do
		errorDialog "Сначала надо сделать ошибку!"
		else do
			labelSetText resl (showPol (decode code pol))	

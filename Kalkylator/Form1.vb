Public Class Form1
	Sub Calculate()
		If (VerifyLegalMath()) Then
			CalculateSquaresAndRoots()
			CalculateMultiplicationAndDivison()
			CalculateAdditionAndSubtraction()
		End If
	End Sub

	Function VerifyLegalMath()
		If (VerifyRootsLegal() And VerifyLastCharLegal()) Then 'Go through all checks
			Return True 'If all legal return true
		End If

		Return False 'Else return false
	End Function

	Function VerifyRootsLegal()
		Dim checkedRoots = 0 'Amount of string already checked, TODO check it doesnt lock up
		While (tbxMain.Text.Substring(checkedRoots).Contains("√")) 'Loop through all roots, dont check any before index checkedRoots
			checkedRoots = InStr(tbxMain.Text.Substring(checkedRoots), "√") 'Get next root
			If (IsNumeric(tbxMain.Text(checkedRoots))) Then 'Make sure root is prepended to number
				Return False ' return false if not
			End If
		End While
		Return True 'If all roots are legal return true
	End Function

	Function VerifyLastCharLegal()
		If (Not (IsNumeric(tbxMain.Text(tbxMain.Text.Length - 1)) Or tbxMain.Text(tbxMain.Text.Length - 1) = "²")) Then 'Last char can only be ² or numeric
			Return False
		End If
		Return True
	End Function

	'calculate x² and √x
	Sub CalculateSquaresAndRoots()
		'While there are values to square
		While (tbxMain.Text.Contains("²") Or tbxMain.Text.Contains("√"))

			'Get pos of next sqaure and root
			'InStr returns 0 when not found
			Dim nextSquare = InStr(tbxMain.Text, "²")
			Dim nextRoot = InStr(tbxMain.Text, "√")

			'If there is a nextSquare and its before next root or there is no root (nextroot = 0 means no root found)
			If (Not nextSquare = 0 AndAlso (nextSquare < nextRoot Or nextRoot = 0)) Then
				CalculateSquare()

				'Reversed, Keep all checks for redundancy
			ElseIf (Not nextRoot = 0 AndAlso (nextSquare > nextRoot Or nextSquare = 0)) Then
				CalculateRoot()
			End If
		End While
	End Sub

	Sub CalculateSquare()
		'Prep next value to sqaure
		'Remove all chars after it
		Dim toSquare = tbxMain.Text.Substring(0, InStr(tbxMain.Text, "²") - 1)
		'Get length
		Dim len = toSquare.Length - 1
		While ((IsNumeric(toSquare(len)) Or toSquare(len) = ",") And len >= 1)
			len -= 1
		End While
		toSquare = toSquare.Substring(len + 1)
		tbxMain.Text = tbxMain.Text.Replace(toSquare + "²", Math.Pow(Double.Parse(toSquare), 2))
	End Sub

	Sub CalculateRoot()
		'Prep next value to root
		'Remove all chars before it
		Dim toRoot = tbxMain.Text.Substring(InStr(tbxMain.Text, "√"))
		'Get length
		Dim len = 0
		While (len < toRoot.Length AndAlso (IsNumeric(toRoot(len)) Or toRoot(len) = "."))
			len += 1
		End While
		toRoot = toRoot.Substring(0, len) 'Remove all chars after it

		tbxMain.Text = tbxMain.Text.Replace("√" + toRoot, Math.Sqrt(Double.Parse(toRoot)))
	End Sub

	Sub CalculateMultiplicationAndDivison()
		While (tbxMain.Text.Contains("X") Or tbxMain.Text.Contains("/"))
			Dim nextMultiplication = InStr(tbxMain.Text, "X")
			Dim nextDivision = InStr(tbxMain.Text, "/")

			'If there is a nextSquare and its before next root or there is no root
			If (Not nextMultiplication = 0 AndAlso (nextMultiplication < nextDivision Or nextDivision = 0)) Then
				CalculateMultiplication()
			ElseIf (Not nextDivision = 0 AndAlso (nextMultiplication > nextDivision Or nextMultiplication = 0)) Then 'Reversed, Keep all checks for redundancy
				CalculateDivision()
			End If
		End While
	End Sub

	Sub CalculateMultiplication()
		'Prep value 1
		'Remove all after val1
		Dim val1 = tbxMain.Text.Substring(0, InStr(tbxMain.Text, "X") - 1)
		'Get val1 length
		Dim len = val1.Length - 1
		While ((Not IsNumeric(val1(len)) Or Not val1(len) = ",") And len >= 1)
			len -= 1
		End While
		val1 = val1.Substring(len) 'Remove all chars before it

		Dim val2 = tbxMain.Text.Substring(InStr(tbxMain.Text, "X"))
		len = 0
		While (len < val2.Length AndAlso (IsNumeric(val2(len)) Or val2(len) = "."))
			len += 1
		End While
		val2 = val2.Substring(0, len) 'Remove all chars after it

		tbxMain.Text = tbxMain.Text.Replace(val1 + "X" + val2, val1 * val2) '{val1}X{val2}... will be replaced by {val1 * val2}...

	End Sub
	Sub CalculateDivision()
		'Prep value 1
		'Remove all after val1
		Dim val1 = tbxMain.Text.Substring(0, InStr(tbxMain.Text, "/") - 1)
		'Get val1 length
		Dim len = val1.Length - 1
		While ((IsNumeric(val1(len)) Or val1(len) = ",") And len >= 1)
			len -= 1
		End While
		val1 = val1.Substring(len) 'Remove all chars before it

		Dim val2 = tbxMain.Text.Substring(InStr(tbxMain.Text, "/"))
		len = 0
		While (len < val2.Length AndAlso (IsNumeric(val2(len)) Or val2(len) = "."))
			len += 1
		End While
		val2 = val2.Substring(0, len) 'Remove all chars after it

		tbxMain.Text = tbxMain.Text.Replace(val1 + "/" + val2, val1 / val2)
	End Sub

	Sub CalculateAdditionAndSubtraction()
		'Loop through all '+' and '-' in string
		While (tbxMain.Text.Contains("+") Or tbxMain.Text.Contains("-"))

			'Get pos of next plus and next minus
			Dim nextAddition = InStr(tbxMain.Text, "+")
			Dim nextSubtraction = InStr(tbxMain.Text, "-")

			'If there is a addition and its before next subtraction or there is no subtraction
			If (Not nextAddition = 0 AndAlso (nextAddition < nextSubtraction Or nextSubtraction = 0)) Then
				CalculateAddition() 'Do addition next
			ElseIf (Not nextSubtraction = 0 AndAlso (nextAddition > nextSubtraction Or nextAddition = 0)) Then 'Reversed, Keep all checks for redundancy
				CalculateSubtraction() 'Do subtraction next
			End If
		End While
	End Sub

	Sub CalculateSubtraction()
		'Prep value 1
		'Remove all after val1
		Dim val1 = tbxMain.Text.Substring(0, InStr(tbxMain.Text, "-") - 1)
		'Get val1 length
		Dim len = val1.Length - 1
		While ((Not IsNumeric(val1(len)) Or Not val1(len) = ",") And len >= 1)
			len -= 1
		End While
		val1 = val1.Substring(len) 'Remove all chars before it

		Dim val2 = tbxMain.Text.Substring(InStr(tbxMain.Text, "-"))
		len = 0
		While (len < val2.Length AndAlso (IsNumeric(val2(len)) Or val2(len) = "."))
			len += 1
		End While
		val2 = val2.Substring(0, len) 'Remove all chars after it

		tbxMain.Text = tbxMain.Text.Replace(val1 + "-" + val2, Double.Parse(val1) - Double.Parse(val2)) '{val1}-{val2}... will be replaced by {val1 - val2}... Cast to be safe

	End Sub
	Sub CalculateAddition()
		'Prep value 1
		'Remove all after val1
		Dim val1 = tbxMain.Text.Substring(0, InStr(tbxMain.Text, "+") - 1)
		'Get val1 length
		Dim len = val1.Length - 1
		While ((Not IsNumeric(val1(len)) Or Not val1(len) = ",") And len >= 1)
			len -= 1
		End While
		val1 = val1.Substring(len) 'Remove all chars before it

		'Get val2 length
		Dim val2 = tbxMain.Text.Substring(InStr(tbxMain.Text, "+"))
		len = 0
		While (len < val2.Length AndAlso (IsNumeric(val2(len)) Or val2(len) = "."))
			len += 1
		End While
		val2 = val2.Substring(0, len) 'Remove all chars after it

		tbxMain.Text = tbxMain.Text.Replace(val1 + "+" + val2, Double.Parse(val1) + Double.Parse(val2)) '+ works as string appender, so we need to cast to double
	End Sub


	Private Sub tbxMain_KeyPress(sender As Object, e As KeyPressEventArgs) Handles tbxMain.KeyPress
		Dim valueLegal = False
		Dim legalKeys = New Char() {"+", "-", "/", "*"}
		'Handle comma, stop multiple commas being writen
		If (e.KeyChar = "." AndAlso Not tbxMain.Text.Contains(".")) Then
			valueLegal = True
		End If
		'Hanlde numbers
		If (IsNumeric(e.KeyChar)) Then
			valueLegal = True
		End If

		'Handle legal keys
		If (legalKeys.Contains(e.KeyChar)) Then
			valueLegal = True
		End If

		If (Not valueLegal) Then
			e.Handled = True
		End If

	End Sub

	Private Sub tbxMain_KeyDown(sender As Object, e As KeyEventArgs) Handles tbxMain.KeyDown
		'TODO do the rest.
		'If input is enter, caclulate
		If (e.KeyCode = Keys.Enter) Then
			Calculate()
		End If
		'If backspace and tbxMain.text isnt empty
		If (e.KeyCode = Keys.Back AndAlso tbxMain.Text IsNot "") Then
			tbxMain.Text = tbxMain.Text.Substring(0, tbxMain.Text.Length - 1)
			tbxMain.Select(tbxMain.Text.Length, 0)
		End If
		'Stop moving of cursor instead of complicating backspacing
		If (e.KeyCode = Keys.Right Or e.KeyCode = Keys.Left Or e.KeyCode = Keys.Up Or e.KeyCode = Keys.Down) Then
			e.Handled = True
		End If
	End Sub

	Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
		tbxMain.Text = "√12X5/3²+5"
	End Sub


	'Rules:
	'1: +, -, /, X, ²
	'2: √,
	'3: numbers
	'4: comma
	Sub buttonClick(buttonValue, rule)
		If (rule = 1) Then
			If (tbxMain.Text IsNot "" AndAlso (IsNumeric(tbxMain.Text(tbxMain.Text.Length - 1)) Or tbxMain.Text(tbxMain.Text.Length - 1) = "²")) Then
				tbxMain.Text += buttonValue
			End If
		ElseIf (rule = 2) Then '√ prepends to value to root, need to check if legal before calculation instead
			tbxMain.Text += buttonValue
		ElseIf (rule = 3) Then
			If (Not tbxMain.Text(tbxMain.Text.Length - 1) = "²") Then 'Cannot append number after ²
				tbxMain.Text += buttonValue
			End If
		ElseIf (rule = 4) Then
			If (IsNumeric(tbxMain.Text(tbxMain.Text.Length - 1))) Then 'Can only append to numbers
				tbxMain.Text += buttonValue
			End If
		End If
    End Sub

	Private Sub btnSquare_Click(sender As Object, e As EventArgs) Handles btnSquare.Click
		buttonClick("²", 1)
	End Sub

	Private Sub btnEqual_Click(sender As Object, e As EventArgs) Handles btnEqual.Click
		Calculate()
	End Sub

	Private Sub btnSquareRoot_Click(sender As Object, e As EventArgs) Handles btnSquareRoot.Click
		buttonClick("√", 2)
	End Sub

	Private Sub btnMultiply_Click(sender As Object, e As EventArgs) Handles btnMultiply.Click
		buttonClick("X", 1)
	End Sub

	Private Sub btnDivision_Click(sender As Object, e As EventArgs) Handles btnDivision.Click
		buttonClick("/", 1)
	End Sub

	Private Sub btnPlus_Click(sender As Object, e As EventArgs) Handles btnPlus.Click
		buttonClick("+", 2)
	End Sub

	Private Sub btnNum0_Click(sender As Object, e As EventArgs) Handles btnNum0.Click
		buttonClick("0", 3)
	End Sub

	Private Sub btnNum1_Click(sender As Object, e As EventArgs) Handles btnNum1.Click
		buttonClick("1", 3)
	End Sub

	Private Sub btnComma_Click(sender As Object, e As EventArgs) Handles btnComma.Click
		buttonClick(",", 4)
	End Sub
End Class
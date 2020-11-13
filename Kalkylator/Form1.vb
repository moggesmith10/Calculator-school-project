Public Class Form1
	Dim CurrentMath As String
	Dim Result As String
	Dim InputGhost As Boolean

	'Obs, binära operationen enligt kravspecen är inte nödvändig enligt muntligt tillåtelse

	Sub Calculate(ghost As Boolean)
		'TODO add check for numbers longer than 8 chars
		Result = CurrentMath
		If (ghost) Then
			Result = Result.Remove(Result.Length - 1) 'If ghost remove last char, i.e +/-/X/etc.
		Else
			Result += tbxMain.Text
		End If
		If (VerifyLegalMath() = "True") Then 'Returns string to allow error message TODO see if there is a typeof() command
			CalculateMultiplicationAndDivison()
			CalculateAdditionAndSubtraction()
		Else
			Result = VerifyLegalMath()
			SetGhost(True)
		End If

		If (ghost) Then
			SetGhost(True)
			tbxMain.Text = Result
			lblCurrentMath.Text = CurrentMath
		Else
			CurrentMath = ""
			tbxMain.Text = Result
			lblCurrentMath.Text = ""
			InputGhost = False
			tbxMain.ForeColor = Color.Black
		End If

	End Sub

	Function VerifyLegalMath()
		If (Not VerifyNoDividesByZero()) Then 'Go through all checks
			Return "E: Div/0" 'If all legal return true
		End If

		Return "True" 'Else return false
	End Function

	Function VerifyNoDividesByZero() '
		If (Result.Contains("/0")) Then
			Return False
		Else
			Return True
		End If
	End Function

	Sub CalculateMultiplicationAndDivison()
		While (Result.Contains("X") Or Result.Contains("/"))
			Dim nextMultiplication = InStr(Result, "X")
			Dim nextDivision = InStr(Result, "/")

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
		Dim val1 = Result.Substring(0, InStr(Result, "X") - 1)
		'Get val1 length
		Dim len = val1.Length - 1
		While ((Not IsNumeric(val1(len)) Or Not val1(len) = ",") And len >= 1)
			len -= 1
		End While
		val1 = val1.Substring(len) 'Remove all chars before it

		Dim val2 = Result.Substring(InStr(Result, "X"))
		len = 0
		While (len < val2.Length AndAlso (IsNumeric(val2(len)) Or val2(len) = ","))
			len += 1
		End While
		val2 = val2.Substring(0, len) 'Remove all chars after it

		Result = Result.Replace(val1 + "X" + val2, val1 * val2) '{val1}X{val2}... will be replaced by {val1 * val2}...

	End Sub
	Sub CalculateDivision()
		'Prep value 1
		'Remove all after val1
		Dim val1 = Result.Substring(0, InStr(Result, "/") - 1)
		'Get val1 length
		Dim len = val1.Length - 1
		While ((IsNumeric(val1(len)) Or val1(len) = ",") And len >= 1)
			len -= 1
		End While
		val1 = val1.Substring(len) 'Remove all chars before it

		Dim val2 = Result.Substring(InStr(Result, "/"))
		len = 0
		While (len < val2.Length AndAlso (IsNumeric(val2(len)) Or val2(len) = ","))
			len += 1
		End While
		val2 = val2.Substring(0, len) 'Remove all chars after it

		Result = Result.Replace(val1 + "/" + val2, val1 / val2)
	End Sub

	Sub CalculateAdditionAndSubtraction()
		'Loop through all '+' and '-' in string
		While (Result.Contains("+") Or Result.Contains("-"))

			'Get pos of next plus and next minus
			Dim nextAddition = InStr(Result, "+")
			Dim nextSubtraction = InStr(Result, "-")

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
		Dim val1 = Result.Substring(0, InStr(Result, "-") - 1)
		'Get val1 length
		Dim len = val1.Length - 1
		While ((Not IsNumeric(val1(len)) Or Not val1(len) = ",") And len >= 1)
			len -= 1
		End While
		val1 = val1.Substring(len) 'Remove all chars before it

		Dim val2 = Result.Substring(InStr(Result, "-"))
		len = 0
		While (len < val2.Length AndAlso (IsNumeric(val2(len)) Or val2(len) = ","))
			len += 1
		End While
		val2 = val2.Substring(0, len) 'Remove all chars after it

		Result = Result.Replace(val1 + "-" + val2, Double.Parse(val1) - Double.Parse(val2)) '{val1}-{val2}... will be replaced by {val1 - val2}... Cast to be safe

	End Sub
	Sub CalculateAddition()
		'Prep value 1
		'Remove all after val1
		Dim val1 = Result.Substring(0, InStr(Result, "+") - 1)
		'Get val1 length
		Dim len = val1.Length - 1
		While ((Not IsNumeric(val1(len)) Or Not val1(len) = ",") And len >= 1)
			len -= 1
		End While
		val1 = val1.Substring(len) 'Remove all chars before it

		'Get val2 length
		Dim val2 = Result.Substring(InStr(Result, "+"))
		len = 0
		While (len < val2.Length AndAlso (IsNumeric(val2(len)) Or val2(len) = ","))
			len += 1
		End While
		val2 = val2.Substring(0, len) 'Remove all chars after it

		Result = Result.Replace(val1 + "+" + val2, Double.Parse(val1) + Double.Parse(val2)) '+ works as string appender, so we need to cast to double
	End Sub

	Sub SetGhost(ghost As Boolean)
		If (ghost) Then
			InputGhost = True
			tbxMain.ForeColor = Color.Gray
		Else
			InputGhost = False
			tbxMain.ForeColor = Color.Black
		End If
	End Sub

	Private Sub tbxMain_KeyPress(sender As Object, e As KeyPressEventArgs) Handles tbxMain.KeyPress
		Dim valueLegal = False
		Dim legalKeys = New Char() {"+", "-", "/", "*"}
		'Handle comma, stop multiple commas being writen
		If (e.KeyChar = "." AndAlso Not tbxMain.Text.Contains(".")) Then
			tbxMain.Text += "."
		End If
		'Hanlde numbers
		If (IsNumeric(e.KeyChar)) Then
			buttonClick(e.KeyChar, 3)
		End If

		'Handle legal keys
		If (legalKeys.Contains(e.KeyChar)) Then
			buttonClick(e.KeyChar, 1)
		End If

		e.Handled = True
		tbxMain.Select(tbxMain.Text.Length, 0)
	End Sub

	Private Sub tbxMain_KeyDown(sender As Object, e As KeyEventArgs) Handles tbxMain.KeyDown
		'TODO do the rest.
		'If input is enter, caclulate
		If (e.KeyCode = Keys.Enter) Then
			Calculate(False)
		End If
		'If backspace and tbxMain.text isnt empty
		If (e.KeyCode = Keys.Back AndAlso tbxMain.Text IsNot "") Then
			If (InputGhost) Then
				tbxMain.Text = ""
			Else
				tbxMain.Text = tbxMain.Text.Substring(0, tbxMain.Text.Length - 1)
			End If
			tbxMain.Select(tbxMain.Text.Length, 0)
		End If
		'Stop moving of cursor instead of complicating backspacing
		If (e.KeyCode = Keys.Right Or e.KeyCode = Keys.Left Or e.KeyCode = Keys.Up Or e.KeyCode = Keys.Down) Then
			e.Handled = True
		End If
	End Sub

	Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
		CurrentMath = ""
		lblCurrentMath.Text = CurrentMath
	End Sub


	'Rules:
	'1: +, -, /, X
	'2: √, ²
	'3: numbers
	'4: comma
	Sub buttonClick(buttonValue, rule)
		If (rule = 1) Then
			If (tbxMain.Text IsNot "" AndAlso (IsNumeric(tbxMain.Text(tbxMain.Text.Length - 1)) Or tbxMain.Text(tbxMain.Text.Length - 1) = "²")) Then
				CurrentMath += tbxMain.Text + buttonValue
				Calculate(True)
			End If
		ElseIf (rule = 2) Then 'Roots and squares happen instantly
			If (buttonValue = "√") Then
				tbxMain.Text = Math.Sqrt(tbxMain.Text)
			ElseIf (buttonValue = "²") Then
				tbxMain.Text = Math.Pow(tbxMain.Text, 2)
			End If
			SetGhost(False)

		ElseIf (rule = 3) Then
			If (tbxMain.Text.Length = 0 OrElse Not tbxMain.Text(tbxMain.Text.Length - 1) = "²") Then 'Cannot append number after ²
				If (InputGhost) Then
					tbxMain.Text = buttonValue
					InputGhost = False
					tbxMain.ForeColor = Color.Black
				Else
					tbxMain.Text += buttonValue
				End If
			End If
		ElseIf (rule = 4) Then
			If (tbxMain.Text = "" Or InputGhost) Then
				tbxMain.Text = "0" + buttonValue
			ElseIf (IsNumeric(tbxMain.Text(tbxMain.Text.Length - 1))) Then 'Can only append to numbers
				tbxMain.Text += buttonValue
			End If
		End If
	End Sub

	Private Sub btnSquare_Click(sender As Object, e As EventArgs) Handles btnSquare.Click
		buttonClick("²", 2)
	End Sub

	Private Sub btnEqual_Click(sender As Object, e As EventArgs) Handles btnEqual.Click
		Calculate(False)
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
		buttonClick("+", 1)
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

	Private Sub btnNum2_Click(sender As Object, e As EventArgs) Handles btnNum2.Click
		buttonClick("2", 3)
	End Sub
End Class
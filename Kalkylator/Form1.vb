﻿Public Class Form1
	Dim CurrentMath As String
	Dim Result As String
	Dim InputGhost As Boolean
	Dim Memory As Double = 0
	Dim KeysDown As New List(Of Keys)

	'Obs, binära operationen enligt kravspecen är inte nödvändig enligt muntligt tillåtelse

	'Dirty
	Sub Calculate(ghost As Boolean)
		'TODO add check for numbers longer than 8 chars
		Result = CurrentMath
		If (ghost) Then
			Result = Result.Remove(Result.Length - 1) 'If ghost remove last char, i.e +/-/X/etc.
		Else
			If (tbxMain.Text.Contains("E")) Then 'Remove E for errors or incase somehow more numbers than allowed is input (1.235E...)
				tbxMain.Text = "0"
			End If

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
			SetGhost(False)
		End If

	End Sub

	Function VerifyLegalMath()
		If (Not VerifyNoDividesByZero()) Then 'Go through all checks
			Return "E: Div/0" 'Error: Divide By Zero
		End If

		'Legalize some math
		While (Result.Contains("--"))
			Result = Result.Replace("--", "+")
		End While

		Return "True" 'If all legal return true (string so we can return error messages)
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
		While ((Not IsNumeric(val1(len)) Or val1(len) = ",") And len >= 1)
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
		val1 = val1.Substring(len + 1) 'Remove all chars before it

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
		While (Result.Length > 0 AndAlso (Result.Contains("+") Or Result.Substring(1).Contains("-"))) 'First char cant be minus, just means negative number, dont run incase empty because substring

			'Get pos of next plus and next minus
			Dim nextAddition = InStr(Result, "+")
			Dim nextSubtraction = InStr(Result.Substring(1), "-") + 1 'First char cant be minus, just means negative number

			If (Not Result.Contains("-")) Then 'Substring 1 + 1 works whenever there is a minus, but not when there isn't
				nextSubtraction = 0
			End If

			'If there is a addition and its before next subtraction or there is no subtraction
			If (Not nextAddition = 0 AndAlso (nextAddition < nextSubtraction Or nextSubtraction = 0)) Then
				CalculateAddition() 'Do addition next
			ElseIf (Not nextSubtraction = 0 AndAlso (nextAddition > nextSubtraction Or nextAddition = 0)) Then 'Reversed, Keep all checks for redundancy, not 1 for negative numbers
				CalculateSubtraction() 'Do subtraction next
			End If
		End While
	End Sub

	Sub CalculateSubtraction()
		'Prep value 1
		'Remove all after val1
		Dim val1 = Result.Substring(0, InStr(Result.Substring(1), "-")) 'Remove first char incase negative numbers, no need to +1 due to counting from 0. (used to be -1 before substring was implemented)
		If (val1 = "") Then
			val1 = 0 'Safety first kiddos
		End If
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
		While (len < val2.Length AndAlso (IsNumeric(val2(len)) Or val2(len) = "," Or val2(len) = "-"))
			len += 1
		End While
		val2 = val2.Substring(0, len) 'Remove all chars after it
		If (val2 = "") Then ' val2 can become empty with some Clear trickery
			Result += 0
			val2 = 0
		End If

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

	'Dirty
	Private Sub tbxMain_KeyPress(sender As Object, e As KeyPressEventArgs) Handles tbxMain.KeyPress
		Dim legalKeys = New Char() {"+", "-", "/", "*", "C"}
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
			If (KeysDown.Contains(Keys.ControlKey)) Then
				If (e.KeyChar = "/") Then
					buttonClick("1/X", 2)
				End If
			Else
				If (e.KeyChar = "*") Then '* is the required keyboard input, X is used to show it
					buttonClick("X", 1)
				ElseIf (e.KeyChar = "C") Then 'C should never be written to textbox
					Clear()
				Else
					buttonClick(e.KeyChar, 1)
				End If
			End If
		End If

		e.Handled = True
		tbxMain.Select(tbxMain.Text.Length, 0)
	End Sub

	'Dirty
	Private Sub tbxMain_KeyDown(sender As Object, e As KeyEventArgs) Handles tbxMain.KeyDown
		'TODO do the rest.
		KeysDown.Add(e.KeyCode)
		'If input is enter, caclulate
		If (e.KeyCode = Keys.Enter) Then
			Calculate(False)
		End If
		If (e.KeyCode = Keys.Escape) Then
			AllClear()
		End If
		If (KeysDown.Contains(Keys.ControlKey)) Then
			If (e.KeyCode = Keys.Multiply) Then 'Control and star is buggy, needs to be here since it doesnt print
				buttonClick("²", 2)
			ElseIf (e.KeyCode = Keys.Divide) Then
				buttonClick("1/X", 2)
			End If

		End If
		'If backspace and tbxMain.text isnt empty
		If (e.KeyCode = Keys.Back AndAlso tbxMain.Text IsNot "") Then
			If (InputGhost) Then
				tbxMain.Text = ""
				SetGhost(False) 'Unecesary but good for redundancy, currently sets non existent text to black so no noticiable difference
			Else
				tbxMain.Text = tbxMain.Text.Substring(0, tbxMain.Text.Length - 1)
			End If
			tbxMain.Select(tbxMain.Text.Length, 0)
		End If
		'If (e.KeyCode = Keys.) Then
		'Stop moving of cursor instead of complicating backspacing
		If (e.KeyCode = Keys.Right Or e.KeyCode = Keys.Left Or e.KeyCode = Keys.Up Or e.KeyCode = Keys.Down) Then
			e.Handled = True
		End If
	End Sub

	Private Sub tbxMain_KeyUp(sender As Object, e As KeyEventArgs) Handles tbxMain.KeyUp
		KeysDown.Remove(e.KeyCode)
	End Sub

	Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
		CurrentMath = ""
		lblCurrentMath.Text = CurrentMath
		lblMem.Text = ""
	End Sub


	'Rules:
	'1: +, -, /, X
	'2: √, ², +/-
	'3: numbers
	'4: comma
	Sub buttonClick(buttonValue, rule)
		If (rule = 1) Then
			WriteOperators(buttonValue)
		ElseIf (rule = 2) Then 'Roots and squares happen instantly
			WriteUnaryOperators(buttonValue)
		ElseIf (rule = 3) Then
			WriteNumbers(buttonValue)
		ElseIf (rule = 4) Then
			WriteComma(buttonValue)
		End If
	End Sub

	Sub WriteOperators(buttonValue)
		If (tbxMain.Text IsNot "" AndAlso (IsNumeric(tbxMain.Text(tbxMain.Text.Length - 1)) Or tbxMain.Text(tbxMain.Text.Length - 1) = "²")) Then
			CurrentMath += tbxMain.Text + buttonValue
			Calculate(True)
		End If
	End Sub

	Sub WriteUnaryOperators(buttonValue)
		If (buttonValue = "√") Then
			If (Double.Parse(tbxMain.Text) > 0) Then 'Dont root 0 or negative numbers
				tbxMain.Text = Math.Sqrt(tbxMain.Text)
			Else
				tbxMain.Text = "E: Irr" 'u ohh, it seems u should have done some legal math there buddy, too bad its gone now
				SetGhost(True)
			End If
		ElseIf (buttonValue = "²") Then
			tbxMain.Text = Math.Pow(tbxMain.Text, 2) 'Can be used without Math class if needed
		ElseIf (buttonValue = "+/-") Then
			tbxMain.Text = -Double.Parse(tbxMain.Text)
		ElseIf (buttonValue = "1/X") Then
			tbxMain.Text = 1 / Double.Parse(tbxMain.Text)
		End If
		SetGhost(True) ' Why kravspec gotta be so cruel
	End Sub

	Sub WriteNumbers(buttonValue)
		If (InputGhost) Then
			tbxMain.Text = buttonValue
			SetGhost(False)
		ElseIf (tbxMain.Text.Length < 8) Then 'Maxlength of input is 8 chars
			tbxMain.Text += buttonValue
		End If
	End Sub

	Sub WriteComma(buttonValue) 'Keep buttonValue for redundancy, should make it easier to globalize (i.e , -> .)
		If (tbxMain.Text = "" Or InputGhost) Then
			tbxMain.Text = "0" + buttonValue
			SetGhost(False)
		ElseIf (IsNumeric(tbxMain.Text(tbxMain.Text.Length - 1))) Then 'Can only append to numbers
			tbxMain.Text += buttonValue
			SetGhost(False)
		End If
	End Sub

	Sub Clear()
		tbxMain.Text = ""
	End Sub

	Sub AllClear()
		tbxMain.Text = ""
		lblCurrentMath.Text = ""
		CurrentMath = ""
		Result = ""
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

	Private Sub btnSubtract_Click(sender As Object, e As EventArgs) Handles btnSubtract.Click
		buttonClick("-", 1)
	End Sub

	Private Sub btnNum3_Click(sender As Object, e As EventArgs) Handles btnNum3.Click
		buttonClick("3",3)
	End Sub

	Private Sub btnNum4_Click(sender As Object, e As EventArgs) Handles btnNum4.Click
		buttonClick("4", 3)
	End Sub

	Private Sub btnNum5_Click(sender As Object, e As EventArgs) Handles btnNum5.Click
		buttonClick("5",3)
	End Sub

	Private Sub btnNum6_Click(sender As Object, e As EventArgs) Handles btnNum6.Click
		buttonClick("6", 3)
	End Sub

	Private Sub btnNum7_Click(sender As Object, e As EventArgs) Handles btnNum7.Click
		buttonClick("7", 3)
	End Sub

	Private Sub btnNum8_Click(sender As Object, e As EventArgs) Handles btnNum8.Click
		buttonClick("8", 3)
	End Sub

	Private Sub btnNum9_Click(sender As Object, e As EventArgs) Handles btnNum9.Click
		buttonClick("9", 3)
	End Sub

	Private Sub btnFlip_Click(sender As Object, e As EventArgs) Handles btnFlip.Click
		buttonClick("+/-", 2)
	End Sub

	Private Sub btnClear_Click(sender As Object, e As EventArgs) Handles btnClear.Click
		Clear()
	End Sub

	Private Sub btnAllClear_Click(sender As Object, e As EventArgs) Handles btnAllClear.Click
		AllClear()
	End Sub

	Private Sub btnMemReturn_Click(sender As Object, e As EventArgs) Handles btnMemReturn.Click
		tbxMain.Text = Memory
	End Sub

	Private Sub btnMemAdd_Click(sender As Object, e As EventArgs) Handles btnMemAdd.Click
		Calculate(False)
		Memory = Double.Parse(Memory) + Double.Parse(Result)
		SetGhost(True)
		lblMem.Text = "Mem: " + Memory.ToString()
	End Sub

	Private Sub btnMemClear_Click(sender As Object, e As EventArgs) Handles btnMemClear.Click
		Memory = 0
		lblMem.Text = ""
	End Sub

	Private Sub btnDivideByX_Click(sender As Object, e As EventArgs) Handles btnDivideByX.Click
		buttonClick("1/X", 2)
	End Sub
End Class
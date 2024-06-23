using System.Linq.Dynamic.Core;

namespace pysharp;

/* __Known bugs:__
 * Crashes if a user defined function doesn't have a return
 * Arrays can not be created without at least one element in them
 */

public class Interpreter
{
    // due to the variables dict being non static, it means that the current architecture won't properly allow for scoping
    private readonly Dictionary<string, object?> variables = new();
    private Dictionary<string, Delegate> inbuiltFunctions = new();

    // the function name links to an array of variable names, and then the code
    private Dictionary<string, (string[], string)> userDefinedFunctions = new();

    public Interpreter()
    {
        InitializeFunctions();
    }

    private bool? EvaluateBooleanExpression(string expression)
    {
        try
        {
            ParsingConfig config = new() { AllowNewToEvaluateAnyType = true };
            var result = DynamicExpressionParser.ParseLambda(config, [], typeof(bool), expression, variables).Compile().DynamicInvoke();
            return (bool)result;
        }
        catch { return null; }
    }

    private double? EvaluateMathematicalExpression(string input)
    {
        NCalc.Expression expression = new(input);

        try
        {
            // assign the variables from the dictionary to the expression parameters
            foreach (var variable in variables)
            {
                expression.Parameters[variable.Key] = variable.Value;
            }

            return Convert.ToDouble(expression.Evaluate());
        }

        catch { return null; }
    }

    #region Inbuilt Functions
    private static object Print(object[] args)
    {
        string text = Convert.ToString(args[0]);

        Console.WriteLine(text);
        return true;
    }

    private object Dereference(object[] args)
    {
        string variableName = Convert.ToString(args[0]);
        variables.Remove(variableName);
        return true;
    }

    private static object Stringify(object[] args)
    {
        object trueValue = args[0];
        string stringValue = trueValue.ToString();

        if (trueValue.GetType() == typeof(List<object>))
        {
            List<object> array = (List<object>)trueValue;
            stringValue = '[' + string.Join(", ", array.Select(x => x.ToString()).ToArray()) + ']';
        }

        return stringValue;
    }

    private static object? Sleep(object[] args)
    {
        try
        {
            int delay = (int)args[0];
            string factor = (string)args[1];
            switch (factor)
            {
                case "s": delay *= 1000; break;
                case "m": delay *= 60_000; break;
                case "h": delay *= 3_600_000; break;
                default: break;
            }

            Thread.Sleep(delay);
            return true;
        }
        catch { return null; }
    }

    private static object? Root(object[] args)
    {
        try
        {
            float x = Convert.ToSingle(args[0]);
            float n = Convert.ToSingle(args[1]);

            return MathF.Pow(x, 1 / n);
        }
        catch { return null; }
    }

    private static object? Sin(object[] args)
    {
        try
        {
            float n = Convert.ToSingle(args[0]);
            return MathF.Sin(n);
        }
        catch { return null; }
    }

    private static object? Cos(object[] args)
    {
        try
        {
            float n = Convert.ToSingle(args[0]);
            return MathF.Cos(n);
        }
        catch { return null; }
    }
    private static object? Tan(object[] args)
    {
        try
        {
            float n = Convert.ToSingle(args[0]);
            return MathF.Tan(n);
        }
        catch { return null; }
    }

    private static object? Asin(object[] args)
    {
        try
        {
            float n = Convert.ToSingle(args[0]);
            return MathF.Asin(n);
        }
        catch { return null; }
    }

    private static object? Acos(object[] args)
    {
        try
        {
            float n = Convert.ToSingle(args[0]);
            return MathF.Acos(n);
        }
        catch { return null; }
    }

    private static object? Atan(object[] args)
    {
        try
        {
            float n = Convert.ToSingle(args[0]);
            return MathF.Atan(n);
        }
        catch { return null; }
    }

    private object? Input(object[] args)
    {
        string prompt = Convert.ToString(args[0]);
        Console.Write(prompt);

        string userInput = Console.ReadLine();
        if (!TryParseVariableValue(userInput, out object? value))
        {
            value = userInput;
        }

        return value;
    }

    private object? Length(object[] args)
    {
        object trueValue = args[0];

        if (trueValue is List<object>)
        {
            List<object> list = trueValue as List<object>;
            return list.Count;
        }

        return Convert.ToString(trueValue).Length;
    }

    private object? ReadFile(object[] args)
    {
        string path = Convert.ToString(args[0]);

        if (File.Exists(path)) { return File.ReadAllText(path); }

        throw new Exception($"Error: file '{path}' does not exist.");
    }

    private object WriteFile(object[] args)
    {
        string path = Convert.ToString(args[0]);
        string text = Convert.ToString(args[1]);

        File.WriteAllText(path, text);
        return true;
    }
    #endregion

    #region String Handling
    private static bool IsString(string input)
    {
        return (input.StartsWith('"') || input.StartsWith("f\"")) && input.EndsWith('"');
    }

    private string FormatString(string input)
    {
        if (input.StartsWith('"'))
        {
            return input.TrimStart('"').Trim('\"');
        }
        else
        {
            // remove the leading 'f' and the quotes
            input = input.TrimStart('f').Trim('\"');

            // find and replace all placeholders with their corresponding values from the dictionary
            foreach (KeyValuePair<string, object?> variable in variables)
            {
                string value = variable.Value.ToString();

                if (variable.Value.GetType() == typeof(List<object>))
                {
                    List<object> array = (List<object>)variable.Value;
                    value = '[' + string.Join(", ", array.Select(x => x.ToString()).ToArray()) + ']';
                }

                input = input.Replace($"{{{variable.Key}}}", value);
            }

            return input;
        }
    }
    #endregion

    private bool TryParseVariableValue(string value, out object? variableValue)
    {
        variableValue = value switch
        {
            var v when int.TryParse(v, out int variableInt) => variableInt,
            var v when float.TryParse(v, out float variableFloat) => variableFloat,
            var v when bool.TryParse(v, out bool variableBool) => variableBool,
            var v when IsString(v) => FormatString(v),
            _ => null
        };

        return variableValue != null;
    }

    private List<object>? HandleListAssignment(string givenVariableValue)
    {
        if (givenVariableValue.StartsWith('[') && givenVariableValue.EndsWith(']'))
        {
            string items = givenVariableValue[1..^1];

            string[] naiveArray = ParseArguments(items);

            object[] trueArray = new object[naiveArray.Length];
            for (int i = 0; i < naiveArray.Length; i++)
            {
                string item = naiveArray[i];

                if (!GetValueOfToken(new Queue<string>(item.Split()), out trueArray[i])) { return null; }
            }

            return trueArray.ToList();
        }

        return null;
    }

    private object? HandleListAccess(string givenVariableValue)
    {
        if (givenVariableValue.Contains('[') && givenVariableValue.EndsWith(']'))
        {
            string variableName = givenVariableValue[..givenVariableValue.IndexOf('[')];
            if (!variables.TryGetValue(variableName, out object value)) { return null; }

            string attemptedIndex = givenVariableValue[(givenVariableValue.IndexOf('[') + 1)..givenVariableValue.IndexOf(']')];

            if (!GetValueOfToken(new Queue<string>(attemptedIndex.Split()), out object index)) { return null; }
            if (!int.TryParse(index.ToString(), out int intIndex)) { return null; }

            List<object> list = value as List<object>;

            return list[intIndex];
        }

        return null;
    }

    private bool GetValueOfToken(Queue<string> tokens, out object? trueVariableValue)
    {
        string givenVariableValue = string.Join(' ', tokens);

        if (givenVariableValue.StartsWith('@'))
        {
            trueVariableValue = givenVariableValue[1..];

            if (variables.ContainsKey((string)trueVariableValue)) { return true; }

            throw new Exception($"An error occurred: variable '{trueVariableValue}' does not exist.");
        }

        if (TryParseVariableValue(givenVariableValue, out trueVariableValue)) { return true; }

        // else, attempt to get value from function
        trueVariableValue = GetValueFromFunction(givenVariableValue);
        if (trueVariableValue is not null) { return true; }

        trueVariableValue = HandleListAssignment(givenVariableValue);
        if (trueVariableValue is not null) { return true; }

        trueVariableValue = HandleListAccess(givenVariableValue);
        if (trueVariableValue is not null) { return true; }

        if (givenVariableValue.Contains('.'))
        {
            string variableName = givenVariableValue[..givenVariableValue.IndexOf('.')];
            string methodNameAndArgs = givenVariableValue[(givenVariableValue.IndexOf('.') + 1)..];
            string methodName = methodNameAndArgs[..methodNameAndArgs.IndexOf('(')];

            string[] args = ParseArguments(methodNameAndArgs[(methodNameAndArgs.IndexOf('(') + 1)..^1]);

            return TryExecuteMethod(variableName, methodName, args, out trueVariableValue);
        }

        trueVariableValue = EvaluateBooleanExpression(givenVariableValue);
        if (trueVariableValue is not null) { return true; }

        trueVariableValue = EvaluateMathematicalExpression(givenVariableValue);
        if (trueVariableValue is not null) { return true; }

        return variables.TryGetValue(givenVariableValue, out trueVariableValue);
    }

    // this allows for fully recursive arguments
    // ie: a string of "\"add\", Sum(27, Sub(56, 2)), 3, 8"
    // will correctly return an array of:
    /* 
     * - "add"
     * - Sum(27, Sub(56, 2))
     * - 3
     * - 8
     */
    // and on the second index, the same algorithm will parse Sum(27, Sub(56, 2)) as:
    /* 
     * - 27
     * - Sub(56, 2)
     */
    // and so on and so forth..
    private static string[] ParseArguments(string arguments)
    {
        var args = new List<string>();
        bool inQuotes = false;
        int depth = 0;
        int start = 0;

        for (int i = 0; i < arguments.Length; i++)
        {
            switch (arguments[i])
            {
                case '"':
                    inQuotes = !inQuotes;
                    break;
                case '(':
                    if (!inQuotes) { depth++; }
                    break;
                case ')':
                    if (!inQuotes) { depth--; }
                    break;
                case '[':
                    if (!inQuotes) { depth++; }
                    break;
                case ']':
                    if (!inQuotes) { depth--; }
                    break;
                case ',':
                    if (!inQuotes && depth == 0)
                    {
                        args.Add(arguments[start..i].Trim());
                        start = i + 1;
                    }
                    break;
            }
        }

        // add the last token
        args.Add(arguments[start..].Trim());

        return args.ToArray();
    }

    private object? HandleListMethods(List<object> list, string methodName, string[] args)
    {
        switch (methodName)
        {
            case "sort":
                list.Sort();
                return list;

            case "reverse":
                list.Reverse();
                return list;

            case "append":
                if (args.Length == 1 && GetValueOfToken(new Queue<string>(args[0].Split()), out object value)) { list.Add(value); }
                return list;

            case "contains":
                if (args.Length == 1 && GetValueOfToken(new Queue<string>(args[0].Split()), out value)) { return list.Contains(Convert.ToDouble(value)); }
                break;

            case "stringify":
                return Stringify([list]);
        }
        return null;
    }

    private object? HandleStringMethods(string str, string methodName, string[] args)
    {
        switch (methodName)
        {
            case "upper":
                return str.ToUpper();

            case "lower":
                return str.ToLower();

            case "reverse":
                return str.Reverse();

            case "contains":
                if (args.Length == 1 && GetValueOfToken(new Queue<string>(args[0].Split()), out object value)) { return str.Contains((string)value); }
                break;
        }

        return null;
    }

    private bool TryExecuteMethod(string variableName, string methodName, string[] methodArgs, out object? result)
    {
        if (!GetValueOfToken(new Queue<string>(variableName.Split()), out object target))
        {
            result = null;
            return false;
        }

        try
        {
            switch (target)
            {
                case List<object> var:
                    result = HandleListMethods(var, methodName, methodArgs);
                    return result != null;

                case string var:
                    result = HandleStringMethods(var, methodName, methodArgs);
                    return result != null;

                default:
                    result = null;
                    return false;
            }
        }
        catch
        {
            result = null;
            return false;
        }
    }

    private object? GetValueFromFunction(string line)
    {
        int indexOfOpenBrace = line.IndexOf('(');
        int indexOfCloseBrace = line.IndexOf(')');

        if (indexOfOpenBrace == -1 || indexOfCloseBrace == -1) return null;

        string referencedFunction = line[..indexOfOpenBrace];

        string arguments = line[(indexOfOpenBrace + 1)..^1];
        string[] args = ParseArguments(arguments);
        object[] trueArgs = new object[args.Length];

        // evaluate args into a variable reference or else
        for (int i = 0; i < args.Length; i++)
        {
            string arg = args[i];

            if (string.IsNullOrWhiteSpace(arg)) { break; }

            // try to get the value from the variables dictionary. If that doesn't work: attempt a parse
            if (!GetValueOfToken(new Queue<string>(arg.Split()), out object value))
            {
                return null;
            }

            // assign the retrieved or parsed value back to the args array
            trueArgs[i] = value;
        }

        if (trueArgs[0] is null) { trueArgs[0] = string.Empty; }

        if (inbuiltFunctions.TryGetValue(referencedFunction, out var function))
        {
            var castedFunction = (Func<object[], object>)function;
            return castedFunction(trueArgs);
        }
        else if (userDefinedFunctions.TryGetValue(referencedFunction, out var userDefinedFunction))
        {
            Dictionary<string, object?> functionVariables = new();
            for (int i = 0; i < trueArgs.Length; i++)
            {
                functionVariables[userDefinedFunction.Item1[i]] = trueArgs[i];
            }

            try
            {
                if (GetValueOfToken(new Queue<string>(Interpret(userDefinedFunction.Item2, functionVariables, true).Split()), out object? result))
                {
                    // unfortunate that we have to do this (due to lack of scoping) but we must also remove the variables from the global scope
                    foreach (KeyValuePair<string, object?> kvp in functionVariables)
                    {
                        variables.Remove(kvp.Key);
                    }

                    return result;
                }
                if (result is not null)
                {
                    throw new Exception($"An error occurred when attempting to execute function {referencedFunction}: the returned value cannot be parsed.");
                }
                return 0;
            }
            catch (ArgumentException) { return 0; }
        }

        return null;
    }

    // this will only work if the curly brace is on the next line
    private (Queue<string>, string) ParseCodeInCurlyBraces(ref Queue<string> code, ref int lineNumber)
    {
        Queue<string> scope = new();
        // remove initial '{'
        code.Dequeue();

        int curlyBraceDepth = 1;
        while (curlyBraceDepth > 0 && code.Count > 0)
        {
            string line = code.Dequeue().Trim();
            lineNumber++;

            curlyBraceDepth += line switch
            {
                "{" => 1,
                "}" => -1,
                _ => 0
            };

            if (curlyBraceDepth != 0) { scope.Enqueue(line); }
        }

        return (code, string.Join('\n', scope));
    }

    private void InitializeFunctions()
    {
        // object[] represents function arguments
        inbuiltFunctions = new()
        {
            { "print", new Func<object[], object>(Print) },
            { "sleep", new Func<object[], object>(Sleep) },
            { "root", new Func<object[], object>(Root) },
            { "sin", new Func<object[], object>(Sin) },
            { "cos", new Func<object[], object>(Cos) },
            { "tan", new Func<object[], object>(Tan) },
            { "asin", new Func<object[], object>(Asin) },
            { "acos", new Func<object[], object>(Acos) },
            { "atan", new Func<object[], object>(Atan) },
            { "input", new Func<object[], object>(Input) },
            { "len", new Func<object[], object>(Length) },
            { "stringify", new Func<object[], object>(Stringify) },
            { "ReadFile", new Func<object[], object>(ReadFile) },
            { "WriteFile", new Func<object[], object>(WriteFile) },
            { "deref", new Func<object[], object>(Dereference) }
        };
    }

    private void SkipElseIfElseBlocks(ref Queue<string> codeLines, ref int lineNumber)
    {
        while (codeLines.Count > 0)
        {
            string nextLine = codeLines.Peek().Trim();
            if (nextLine.StartsWith("else if") || nextLine.StartsWith("else"))
            {
                codeLines.Dequeue(); // Remove the else if / else line
                                     // Skip the scope
                ParseCodeInCurlyBraces(ref codeLines, ref lineNumber);
            }
            else
            {
                break;
            }
        }
    }

    private void HandleElseIfElseBlocks(ref Queue<string> codeLines, ref int lineNumber)
    {
        while (codeLines.Count > 0)
        {
            string nextLine = codeLines.Peek().Trim();
            if (nextLine.StartsWith("else if"))
            {
                codeLines.Dequeue(); // Remove the else if line
                string elseIfInstruction = nextLine[8..].Trim(); // Get the condition
                if (elseIfInstruction.Contains('(') && elseIfInstruction.Contains(')'))
                {
                    string expression = elseIfInstruction[1..^1];
                    (Queue<string>, string) scope = ParseCodeInCurlyBraces(ref codeLines, ref lineNumber);
                    bool? evaluatedExpression = EvaluateBooleanExpression(expression) ?? throw new Exception($"Invalid boolean expression for else if statement: '{elseIfInstruction}'.");
                    if ((bool)evaluatedExpression)
                    {
                        string error = Interpret(scope.Item2);
                        if (error != "Code executed successfully.") throw new Exception(error);
                        // Skip any following else if or else blocks
                        SkipElseIfElseBlocks(ref codeLines, ref lineNumber);
                        return;
                    }
                }
                else
                {
                    throw new Exception($"Error parsing braces for else if statement: '{elseIfInstruction}'.");
                }
            }
            else if (nextLine.StartsWith("else"))
            {
                codeLines.Dequeue(); // Remove the else line
                (Queue<string>, string) scope = ParseCodeInCurlyBraces(ref codeLines, ref lineNumber);
                string error = Interpret(scope.Item2);
                if (error != "Code executed successfully.") throw new Exception(error);
                return;
            }
            else
            {
                break;
            }
        }
    }

    public string Interpret(string code, Dictionary<string, object?>? tempVariables = null, bool isFromUserFunction = false)
    {
        // join the two dictionaries if necessary
        if (tempVariables is not null)
        {
            foreach (KeyValuePair<string, object?> kvp in tempVariables)
            {
                variables[kvp.Key] = kvp.Value;
            }
        }

        // convert all lines into an iterable queue
        Queue<string> codeLines = new(code.Split('\n'));

        int lineNumber = 0;
        while (codeLines.Count > 0)
        {
            string line = codeLines.Dequeue().Trim();
            lineNumber++;

            // check if blank line or comment
            if (string.IsNullOrWhiteSpace(line) || line.StartsWith("//"))
            {
                lineNumber++;
                continue;
            }

            Queue<string> tokens = new(line.Split(' '));

            // the first item in the tokens array will determine the instruction of the line
            string instruction = tokens.Dequeue();

            try
            {
                switch (instruction)
                {
                    // create variable
                    case "var":
                        string variableName = tokens.Dequeue();
                        // remove '='
                        tokens.Dequeue();

                        // we need to join queue to a string, as the variable type may be a string, which includes spaces
                        string givenVariableValue = string.Join(' ', tokens);

                        if (!GetValueOfToken(tokens, out object? trueVariableValue))
                        {
                            throw new Exception($"Error parsing variable type at line: {lineNumber} for variable: '{variableName}', holding value: '{givenVariableValue}'.");
                        }

                        variables[variableName] = trueVariableValue;
                        break;

                    // assignment to variable
                    case string key when variables.ContainsKey(key):
                        // remove '='
                        tokens.Dequeue();

                        givenVariableValue = string.Join(' ', tokens);

                        if (!GetValueOfToken(tokens, out trueVariableValue))
                        {
                            throw new Exception($"Error parsing variable type at line: {lineNumber} for variable: '{key}', holding value: '{givenVariableValue}'.");
                        }

                        variables[key] = trueVariableValue;
                        break;

                    // assignment to array at index
                    case string array when array.Contains('[') && array.EndsWith(']'):
                        // remove '='
                        tokens.Dequeue();

                        variableName = array[..array.IndexOf('[')];
                        if (!variables.TryGetValue(variableName, out object value)) { throw new Exception($"Error at line: {lineNumber}, variable '{variableName}' does not exist."); }

                        string attemptedIndex = array[(array.IndexOf('[') + 1)..array.IndexOf(']')];

                        if (!GetValueOfToken(new Queue<string>(attemptedIndex.Split()), out object index)) { throw new Exception($"Error at line: {lineNumber}, value '{attemptedIndex}' cannot be parsed."); }
                        if (!int.TryParse(index.ToString(), out int intIndex)) { throw new Exception($"Error at line: {lineNumber}, value '{index}' is not an integer."); }

                        List<object> list = value as List<object>;

                        if (!GetValueOfToken(tokens, out value)) { throw new Exception($"Error at line: {lineNumber}, value '{tokens.Dequeue()}' could not be parsed."); }

                        try
                        {
                            list[intIndex] = value;
                        }
                        catch (ArgumentOutOfRangeException)
                        {
                            list.Add(value);
                        }

                        variables[variableName] = list;

                        break;

                    // if statement
                    case "if":
                        string ifStatementInstruction = string.Join(' ', tokens);

                        if (ifStatementInstruction.Contains('(') && ifStatementInstruction.Contains(')'))
                        {
                            // get string expression within braces
                            string expression = ifStatementInstruction[1..^1];
                            (Queue<string>, string) scope = ParseCodeInCurlyBraces(ref codeLines, ref lineNumber);

                            bool? evaluatedExpression = EvaluateBooleanExpression(expression) ?? throw new Exception($"Invalid boolean expression at line: {lineNumber} for if statement: '{instruction}'.");
                            if ((bool)evaluatedExpression)
                            {
                                Interpret(scope.Item2);
                                // skip any following else if or else blocks
                                SkipElseIfElseBlocks(ref codeLines, ref lineNumber);
                            }
                            else
                            {
                                // check for else if or else
                                HandleElseIfElseBlocks(ref codeLines, ref lineNumber);
                            }

                            break;
                        }

                        throw new Exception($"Error parsing braces at line: {lineNumber} for if statement: '{instruction}'.");

                    // while loop
                    case "while":
                        string whileLoopInstruction = string.Join(' ', tokens);

                        if (whileLoopInstruction.Contains('(') && whileLoopInstruction.Contains(')'))
                        {
                            // get string expression within braces
                            string expression = whileLoopInstruction[1..^1];
                            (Queue<string>, string) scope = ParseCodeInCurlyBraces(ref codeLines, ref lineNumber);

                            if (EvaluateBooleanExpression(expression) is null)
                            {
                                throw new Exception($"Invalid boolean expression at line: {lineNumber} for while loop: '{instruction}'.");
                            }

                            // we have to evaluate it again every time the loop is called to account for updating variables
                            while ((bool)EvaluateBooleanExpression(expression))
                            {
                                Interpret(scope.Item2);
                            }

                            break;
                        }

                        throw new Exception($"Error parsing braces at line: {lineNumber} for while loop: '{instruction}'.");

                    // for loop
                    case "for":
                        string forLoopInstruction = string.Join(' ', tokens);

                        if (forLoopInstruction.Contains('(') && forLoopInstruction.Contains(')'))
                        {
                            (Queue<string>, string) scope = ParseCodeInCurlyBraces(ref codeLines, ref lineNumber);

                            string[] args = forLoopInstruction[1..^1].Split(';').Select(arg => arg.Trim()).ToArray();

                            string[] forLoopVar = args[0].Split(' ');
                            string forLoopVarName = forLoopVar[0];
                            string forLoopStart = forLoopVar[2];

                            if (!GetValueOfToken(new Queue<string>(forLoopStart.Split()), out object start))
                            {
                                throw new Exception($"Error parsing for loop arguments at line {lineNumber}.");
                            }

                            // this was originally a while loop that evaluated a boolean expression on the fly - but that was very slow (having to evaluate every frame), so I've moved to this
                            string[] forLoopConditions = args[1].Split(' ');
                            string[] operation = args[2].Split(' ');

                            GetValueOfToken(new Queue<string>(operation[2].Split()), out object? operandObject);
                            int operand = Convert.ToInt32(operandObject);
                            string opCode = operation[1];
                            string condition = forLoopConditions[1];
                            GetValueOfToken(new Queue<string>(forLoopConditions[2].Split()), out object? conditionObject);
                            double conditionValue = Convert.ToDouble(conditionObject);

                            var operations = new Dictionary<string, Func<int, int, int>>
                            {
                                ["+"] = (x, y) => x + y,
                                ["-"] = (x, y) => x - y,
                                ["*"] = (x, y) => x * y,
                                ["/"] = (x, y) => x / y,
                            };

                            var conditions = new Dictionary<string, Func<int, double, bool>>
                            {
                                ["<"] = (x, y) => x < y,
                                ["<="] = (x, y) => x <= y,
                                [">"] = (x, y) => x > y,
                                [">="] = (x, y) => x >= y,
                                ["=="] = (x, y) => x == y,
                                ["!="] = (x, y) => x != y
                            };

                            int current = Convert.ToInt32(start);
                            Func<int, int> op = i => operations[opCode](i, operand);
                            Func<int, bool> cond = i => conditions[condition](i, conditionValue);

                            variables[forLoopVarName] = Convert.ToInt32(start);
                            while (cond(current))
                            {
                                variables[forLoopVarName] = current;
                                Interpret(scope.Item2);
                                current = op(current);
                            }

                            // since the Interpreter class' variables dict is not static, we need to remove the for loop variable manually
                            variables.Remove(forLoopVarName);
                            break;
                        }

                        throw new Exception($"Error parsing braces at line: {lineNumber} during for loop: '{instruction}'.");

                    // user defined function
                    case "fn":
                        string functionInstruction = string.Join(' ', tokens);

                        if (functionInstruction.Contains('(') && functionInstruction.Contains(')'))
                        {
                            int indexOfOpenBrace = functionInstruction.IndexOf('(');

                            string functionName = functionInstruction[..indexOfOpenBrace];
                            // get array of function arguments
                            string[] args = functionInstruction[(indexOfOpenBrace + 1)..^1].Split(',').Select(arg => arg.Trim()).ToArray();
                            (Queue<string>, string) functionCode = ParseCodeInCurlyBraces(ref codeLines, ref lineNumber);

                            userDefinedFunctions[functionName] = (args, functionCode.Item2);
                            break;
                        }

                        throw new Exception($"Error parsing braces at line: {lineNumber} during function creation: '{instruction}'.");

                    // returning a value from a user defined function
                    case "return":
                        if (isFromUserFunction)
                        {
                            return string.Join(' ', tokens);
                        }

                        throw new Exception($"An error has occurred at line: {lineNumber}. The 'return' keyword can only be used from within a function.");

                    case string method when method.Contains('.'):
                        method = string.Join(' ', line);

                        variableName = method[..method.IndexOf('.')];
                        string methodNameAndArgs = method[(method.IndexOf('.') + 1)..];
                        string methodName = methodNameAndArgs[..methodNameAndArgs.IndexOf('(')];

                        string[] methodArgs = ParseArguments(methodNameAndArgs[(methodNameAndArgs.IndexOf('(') + 1)..^1]);

                        if (!TryExecuteMethod(variableName, methodName, methodArgs, out object? result))
                        {
                            throw new Exception($"An error has occurred at line: {lineNumber}. The method '{method}' could not be found.");
                        }

                        break;

                    // if all else fails, we attempt to see if the user is referencing a function
                    default:
                        if (line.Contains('(') && line.Contains(')'))
                        {
                            // a return type of null signifies that something went wrong with processing
                            if (GetValueFromFunction(line) is null)
                            {
                                throw new Exception($"Error parsing function name or arguments at line {lineNumber}.");
                            }

                            break;
                        }

                        // unrecognized instruction
                        throw new Exception($"Execution failed at line {lineNumber++}: Unrecognized instruction '{instruction}'.");
                }
            }
            catch (Exception ex)
            {
                throw new Exception($"Error at line {lineNumber}: {ex.Message}", ex);
            }
        }

        return "Code executed successfully.";
    }
}

using System.Linq.Dynamic.Core;
using System.Text.RegularExpressions;

namespace pysharp;

/* __Known bugs:__
 * Crashes if a user defined function doesn't have a return (pseudo-fix: simply return 0 or something at the end)
 * Cannot do a return statement if within a nest (pseudo-fix: set a variable and then return it at the end)
 * Recursive functions don't work
 * Cannot put open curly brace on same line (have to do it on next)
 * After an optimization of mathematical evaluation, scoping is now a bit weird
 * Line number is still askew
 */

public struct ClassDefinition(string[] arguments)
{
    public Dictionary<string, object?> Properties { get; set; } = new();
    public Dictionary<string, (string[], string)> Methods { get; set; } = new();
    public string[] Parameters { get; set; } = arguments;
}

public class DynamicClassInstance(ClassDefinition classDef)
{
    public Dictionary<string, object?> Properties { get; set; } = classDef.Properties;
    public Dictionary<string, (string[], string)> Methods { get; set; } = classDef.Methods;
    public string[] Parameters { get; set; } = classDef.Parameters;
}

public partial class Interpreter
{
    // due to the variables dict being non-static, it means that the current architecture won't properly allow for scoping
    private readonly Stack<Dictionary<string, object?>> _scopeStack = new();
    private Dictionary<string, Delegate> _inbuiltFunctions = new();

    // the function name links to an array of variable names, and then the code
    private readonly Dictionary<string, (string[], string)> _userDefinedFunctions = new();

    private readonly Dictionary<string, ClassDefinition> _userDefinedClasses = new();

    private readonly ParsingConfig _booleanParseConfig = new() { AllowNewToEvaluateAnyType = true };
    
    private Dictionary<string, object?> _globalVariables = new();

    public Interpreter()
    {
        InitializeFunctions();
        EnterScope();
    }

    private void EnterScope()
    {
        _scopeStack.Push(new Dictionary<string, object?>());
    }

    private void ExitScope()
    {
        _globalVariables = new Dictionary<string, object?>();
        
        if (_scopeStack.Count == 0) { return; }
        foreach (var variable in _scopeStack.SelectMany(scope => scope))
        {
            _globalVariables[variable.Key] = variable.Value;
        }
    }

    private bool TryGetVariable(string key, out object? value)
    {
        foreach (var scope in _scopeStack.Reverse())
        {
            if (scope.TryGetValue(key, out value))
            {
                return true;
            }
        }
        value = null;
        return false;
    }
    
    public object? GetVariable(string key)
    {
        foreach (var scope in _scopeStack.Reverse())
        {
            if (scope.TryGetValue(key, out object? value))
            {
                return value;
            }
        }
        
        return null;
    }

    private void SetVariable(string key, object? value)
    {
        foreach (var scope in _scopeStack.Reverse())
        {
            if (!scope.ContainsKey(key)) { continue; }
            scope[key] = value;
            break;
        }

        _scopeStack.Peek()[key] = value;
        _globalVariables[key] = value;
    }

    private bool? EvaluateBooleanExpression(string expression)
    {
        try
        {
            object? result = DynamicExpressionParser.ParseLambda(_booleanParseConfig, [], typeof(bool), expression, _globalVariables)
                .Compile()
                .DynamicInvoke();
            return (bool)result;
        }
        catch { return null; }
    }

    private double? EvaluateMathematicalExpression(string input)
    {
        NCalc.Expression expression = new(input);

        try
        {
            foreach (var variable in _globalVariables)
            {
                expression.Parameters[variable.Key] = variable.Value;
            }

            return Convert.ToDouble(expression.Evaluate());
        }
        catch
        {
            return null;
        }
    }
    
    #region Non-Static Inbuilts
    private object Input(object[] args)
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
    
    private object Dereference(object[] args)
    {
        string variableName = Convert.ToString(args[0]);

        if (_scopeStack.Reverse().Any(scope => scope.Remove(variableName))
            && _globalVariables.Remove(variableName))
        {
            return true; // var found and removed
        }

        throw new Exception($"An error occurred: variable '{variableName}' does not exist.");
    }
    #endregion

    #region String Handling
    private static bool IsString(string input) => (input.StartsWith('"') || input.StartsWith("f\"")) && input.EndsWith('"');

    private string FormatString(string input)
    {
        input = ReplaceEscapeCharacters(input);
        return input.StartsWith('"') ? input.Trim('"') : ReplacePlaceholders(input.TrimStart('f').Trim('"'));
    }

    private string ReplaceEscapeCharacters(string input)
    {
        return input.Replace("\\n", "\n")
            .Replace("\\t", "\t")
            .Replace("\\r", "\r");
    }

    private string ReplacePlaceholders(string input)
    {
        return MyRegex().Replace(input, match =>
        {
            string key = match.Groups[1].Value;
            foreach (var scope in _scopeStack.Reverse())
            {
                if (scope.TryGetValue(key, out object? value))
                {
                    return FormatValue(value);
                }
            }
            return match.Value; // Keep original if not found
        });
    }

    private static string? FormatValue(object value)
    {
        return value switch
        {
            Dictionary<object, object> dict when value is not string => '{' + string.Join(", ", dict.Select(kv => $"{kv.Key}: {kv.Value}")) + '}',
            // IEnumerable because I may add others in the future
            IEnumerable<object> enumerable when value is not string => "[" + string.Join(", ", enumerable.Select( x => x.ToString() ?? "null")) + "]",
            _ => value.ToString()
        };
    }
    #endregion

    private bool TryParseVariableValue(string value, out object? variableValue)
    {
        variableValue = value switch
        {
            _ when int.TryParse(value, out int variableInt) => variableInt,
            _ when float.TryParse(value, out float variableFloat) => variableFloat,
            _ when bool.TryParse(value, out bool variableBool) => variableBool,
            _ when IsString(value) => FormatString(value),
            _ => null
        };

        return variableValue != null;
    }

    private List<object>? HandleListAssignment(string givenVariableValue)
    {
        if (!givenVariableValue.StartsWith('[') || !givenVariableValue.EndsWith(']')) { return null; }
        
        string items = givenVariableValue[1..^1];

        string[] naiveArray = StaticHelpers.ParseArguments(items);
        // check if assignment was empty
        if (string.IsNullOrWhiteSpace(naiveArray[0]))
        {
            return new List<object>();
        }

        object[] trueArray = new object[naiveArray.Length];
        return naiveArray.Where((item, i) => !GetValueOfToken(item, out trueArray[i])).Any() ? null : trueArray.ToList();
    }

    private object? HandleListAccess(string givenVariableValue)
    {
        if (!givenVariableValue.Contains('[') || !givenVariableValue.EndsWith(']')) { return null; }
        
        string variableName = givenVariableValue[..givenVariableValue.IndexOf('[')];
        if (!TryGetVariable(variableName, out object? value) || value is not List<object>) { return null; }

        string attemptedIndex = givenVariableValue[(givenVariableValue.IndexOf('[') + 1)..^1];
        if (!GetValueOfToken(attemptedIndex, out object? index) || 
            !int.TryParse(index.ToString(), out int intIndex)) { return null; }

        if (value is List<object> list)
        {
            return list[intIndex];
        }

        return null;
    }

    private object? InvokeMethod(DynamicClassInstance obj, string methodName, object?[] args)
    {
        if (!obj.Methods.TryGetValue(methodName, out var methodDef))
            throw new Exception($"Method {methodName} not found on object.");
        EnterScope();
        try
        {
            if (args.Length > 0)
            {
                for (int i = 0; i < args.Length; i++)
                {
                    SetVariable(methodDef.Item1[i], args[i]);
                }
            }
            
            foreach (var prop in obj.Properties)
            {
                SetVariable(prop.Key, prop.Value);
            }

            string functionCode = methodDef.Item2;
            GetValueOfToken(Interpret(functionCode, true, obj), out object? result);

            return result;
        }
        finally
        {
            ExitScope();
        }
    }

    private bool TryGetValueFromClass(string tokens, out object? trueVariableValue)
    {
        if (tokens.Contains('(') && tokens.Contains(')'))
        {
            string className = tokens[..tokens.IndexOf('(')];
            string argsString = tokens[(tokens.IndexOf('(') + 1)..^1];
            string[] argsArray = StaticHelpers.ParseArguments(argsString);
            object[] args = new object[argsArray.Length];

            bool isValidArguments = !string.IsNullOrWhiteSpace(argsArray[0]);
            if (isValidArguments)
            {
                for (int i = 0; i < argsArray.Length; i++)
                {
                    if (!GetValueOfToken(argsArray[i], out args[i]))
                    {
                        throw new Exception($"Error parsing argument '{argsArray[i]}' for class '{className}'.");
                    }
                }
            }

            if (_userDefinedClasses.TryGetValue(className, out ClassDefinition classDef))
            {
                DynamicClassInstance dynamicClass = new(classDef);

                if (isValidArguments)
                {
                    // select all properties that need constructing
                    var constructedProperties = classDef.Properties.Where(pair => pair.Value != null && classDef.Parameters.Contains(pair.Value.ToString())).ToDictionary(pair => pair.Key, pair => pair.Value);

                    for (int i = 0; i < constructedProperties.Count; i++)
                    {
                        string key = constructedProperties.ElementAt(i).Key;
                        dynamicClass.Properties[key] = args[i];
                    }
                }
                
                trueVariableValue = dynamicClass;

                return true;
            }
        }

        if (tokens.Contains('.'))
        {
            string variableName = tokens[..tokens.IndexOf('.')];
            string memberNameAndArgs = tokens[(tokens.IndexOf('.') + 1)..];
            string memberName = memberNameAndArgs.Contains('(')
                ? memberNameAndArgs[..memberNameAndArgs.IndexOf('(')]
                : memberNameAndArgs;

            if (!TryGetVariable(variableName, out object? obj) || obj is not DynamicClassInstance pyObj)
            {
                throw new Exception($"Variable '{variableName}' is not an object.");
            }

            if (memberNameAndArgs.Contains('('))
            {
                string argsString = memberNameAndArgs[(memberNameAndArgs.IndexOf('(') + 1)..^1];
                string[] argsArray = StaticHelpers.ParseArguments(argsString);
                object?[] args = new object?[argsArray.Length];

                if (!string.IsNullOrWhiteSpace(argsArray[0]))
                {
                    for (int i = 0; i < argsArray.Length; i++)
                    {
                        if (!GetValueOfToken(argsArray[i], out object? argValue))
                        {
                            throw new Exception($"Error parsing argument '{argsArray[i]}' for method '{memberName}'.");
                        }
                        args[i] = argValue;
                    }
                }
                
                trueVariableValue = InvokeMethod(pyObj, memberName, args);
                return true;
            }

            trueVariableValue = StaticHelpers.GetProperty(pyObj, memberName);
            return true;
        }

        trueVariableValue = null;
        return false;
    }

    private Dictionary<object, object>? HandleDictionaryAssignment(string givenVariableValue)
    {
        if (!givenVariableValue.Contains('{') || !givenVariableValue.EndsWith('}')) { return null; }
        
        givenVariableValue = givenVariableValue[(givenVariableValue.IndexOf('{') + 1)..^1];
        string[] keyValuePairs = StaticHelpers.ParseArguments(givenVariableValue);

        Dictionary<object, object> dictionary = new();

        // check if assignment was empty
        if (string.IsNullOrWhiteSpace(keyValuePairs[0])) { return dictionary; }
            
        foreach (string kvp in keyValuePairs)
        {
            string[] element = kvp.Split(':').Select(arg => arg.Trim()).ToArray();
                
            if (!GetValueOfToken(element[0], out object? key) || !GetValueOfToken(element[1], out object? value)) { return null; }
            dictionary[key] = value;
        }

        return dictionary;
    }
    
    private object? HandleDictionaryLookup(string givenVariableValue)
    {
        if (!givenVariableValue.Contains('[') || !givenVariableValue.EndsWith(']')) { return null; }
        
        string variableName = givenVariableValue[..givenVariableValue.IndexOf('[')];
        if (!TryGetVariable(variableName, out object? value) || value is not Dictionary<object, object> dict) { return null; }

        string attemptedKey = givenVariableValue[(givenVariableValue.IndexOf('[') + 1)..^1];
        return !GetValueOfToken(attemptedKey, out object? key) ? null : dict[key];
    }

    // could probably cache the result of TryGetVariable in future
    private bool GetValueOfToken(string tokens, out object? trueVariableValue, DynamicClassInstance? passedObj = null)
    {
        // the '@' symbol means we are getting information about the variable, not the value
        // this is mainly (atm, only) used in the deref() function
        if (!tokens.StartsWith('@'))
        {
            return TryParseVariableValue(tokens, out trueVariableValue) ||
                   TryGetVariable(tokens, out trueVariableValue) ||
                   TryGetValueFromFunction(tokens, passedObj, out trueVariableValue) ||
                   TryHandleListOperations(tokens, out trueVariableValue) ||
                   TryHandleDictionaryOperations(tokens, out trueVariableValue) ||
                   TryExecuteMethodFromTokens(tokens, out trueVariableValue) ||
                   TryGetValueFromClass(tokens, out trueVariableValue) ||
                   TryEvaluateExpression(tokens, out trueVariableValue);
        }
        
        trueVariableValue = tokens[1..];
        if (TryGetVariable(trueVariableValue.ToString(), out _))
        {
            return true;
        }
        throw new Exception($"An error occurred: variable '{trueVariableValue}' does not exist.");
    }

    private bool TryGetValueFromFunction(string tokens, DynamicClassInstance? passedObj, out object? trueVariableValue)
    {
        trueVariableValue = GetValueFromFunction(tokens, passedObj);
        return trueVariableValue is not null;
    }

    private bool TryHandleListOperations(string tokens, out object? trueVariableValue)
    {
        trueVariableValue = HandleListAssignment(tokens) ?? HandleListAccess(tokens);
        return trueVariableValue is not null;
    }

    private bool TryHandleDictionaryOperations(string tokens, out object? trueVariableValue)
    {
        trueVariableValue = HandleDictionaryAssignment(tokens) ?? HandleDictionaryLookup(tokens);
        return trueVariableValue is not null;
    }

    private bool TryExecuteMethodFromTokens(string tokens, out object? trueVariableValue)
    {
        trueVariableValue = null;
        if (!tokens.Contains('.')) return false;

        try
        {
            string[] parts = tokens.Split('.', 2);
            string variableName = parts[0];
            string methodNameAndArgs = parts[1];
            string methodName = methodNameAndArgs.Substring(0, methodNameAndArgs.IndexOf('('));
            string[] args = StaticHelpers.ParseArguments(methodNameAndArgs[(methodNameAndArgs.IndexOf('(') + 1)..^1]);

            return TryExecuteMethod(variableName, methodName, args, out trueVariableValue);
        }
        catch
        {
            return false;
        }
    }

    private bool TryEvaluateExpression(string tokens, out object? trueVariableValue)
    {
        // try boolean expression first
        bool? boolResult = EvaluateBooleanExpression(tokens);
        if (boolResult is not null)
        {
            trueVariableValue = boolResult.Value;
            return true;
        }

        // if not a boolean, try mathematical expression
        double? mathResult = EvaluateMathematicalExpression(tokens);
        if (mathResult is not null)
        {
            trueVariableValue = mathResult.Value;
            return true;
        }

        // if neither worked, set to null and return false
        trueVariableValue = null;
        return false;
    }

    private object? HandleDictionaryMethods(Dictionary<object, object> dict, string methodName, string[] args)
    {
        switch (methodName)
        {
            case "contains_key":
                if (args.Length == 1 && GetValueOfToken(args[0], out object? value)) { return dict.ContainsKey(value); }
                break;
            
            case "contains_value":
                if (args.Length == 1 && GetValueOfToken(args[0], out value)) { return dict.ContainsValue(value); }
                break;
            
            case "key_at":
                if (!GetValueOfToken(args[0], out object? index) || 
                    !int.TryParse(index.ToString(), out int intIndex))
                {
                    return null;
                }

                return dict.ElementAt(intIndex).Key;
            
            case "value_at":
                if (!GetValueOfToken(args[0], out index) || 
                    !int.TryParse(index.ToString(), out intIndex))
                {
                    return null;
                }

                return dict.ElementAt(intIndex).Value;
            
            case "stringify":
                return '{' + string.Join(", ", dict.Select(kv => $"{kv.Key}: {kv.Value}")) + '}';
        }

        return null;
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
                if (args.Length == 1 && GetValueOfToken(args[0], out object value)) { list.Add(value); }
                return list;

            case "contains":
                if (args.Length == 1 && GetValueOfToken(args[0], out value)) { return list.Contains(Convert.ToDouble(value)); }
                break;
            
            case "slice":
                if (!GetValueOfToken(args[0], out object? start) || !GetValueOfToken(args[1], out object? end) || 
                    !int.TryParse(start.ToString(), out int intStart) || !int.TryParse(end.ToString(), out int intEnd))
                {
                    return null;
                }

                return list[intStart..intEnd];

            case "max":
                return list.Max(Convert.ToDouble);

            case "min":
                return list.Min(Convert.ToDouble);
            
            case "index_of":
                if (args.Length == 1 && GetValueOfToken(args[0], out value)) { return list.IndexOf(value); }
                break;

            case "stringify":
                return '[' + string.Join(", ", list.Select(x => x.ToString()).ToArray()) + ']';
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
            
            case "index_of":
                if (args.Length == 1 && GetValueOfToken(args[0], out object? value)) { return str.IndexOf(value.ToString()); }
                break;
            
            case "slice":
                if (!GetValueOfToken(args[0], out object? start) || !GetValueOfToken(args[1], out object? end) ||
                    !int.TryParse(start.ToString(), out int intStart) || !int.TryParse(end.ToString(), out int intEnd))
                {
                    return null;
                }

                return str[intStart..intEnd];

            case "contains":
                if (args.Length == 1 && GetValueOfToken(args[0], out value)) { return str.Contains((string)value); }
                break;
        }

        return null;
    }

    private bool TryExecuteMethod(string variableName, string methodName, string[] methodArgs, out object? result)
    {
        if (!GetValueOfToken(variableName, out object? target))
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
                
                case Dictionary<object, object> var:
                    result = HandleDictionaryMethods(var, methodName, methodArgs);
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

    // also applies to a method for a UD object
    private object? GetValueFromFunction(string line, DynamicClassInstance? passedObj = null)
    {
        int indexOfOpenBrace = line.IndexOf('(');
        int indexOfCloseBrace = line.IndexOf(')');

        if (indexOfOpenBrace == -1 || indexOfCloseBrace == -1) return null;

        string referencedFunction = line[..indexOfOpenBrace];

        string arguments = line[(indexOfOpenBrace + 1)..^1];
        string[] args = StaticHelpers.ParseArguments(arguments);
        object?[] trueArgs = new object?[args.Length];

        // evaluate args into a variable reference or else
        for (int i = 0; i < args.Length; i++)
        {
            string arg = args[i];

            if (string.IsNullOrWhiteSpace(arg)) { break; }

            // try to get the value from the variables dictionary. If that doesn't work: attempt a parse
            if (!GetValueOfToken(arg, out object? value))
            {
                return null;
            }

            // assign the retrieved or parsed value back to the args array
            trueArgs[i] = value;
        }

        if (_inbuiltFunctions.TryGetValue(referencedFunction, out var function))
        {
            var castedFunction = (Func<object[], object>)function;
            return castedFunction(trueArgs);
        }

        if (_userDefinedFunctions.TryGetValue(referencedFunction, out var userDefinedFunction))
        {
            EnterScope();
            try
            {
                for (int i = 0; i < trueArgs.Length; i++)
                {
                    SetVariable(userDefinedFunction.Item1[i], trueArgs[i]);
                }

                return GetValueOfToken(Interpret(userDefinedFunction.Item2, true), out object? result) ? result : 0;
            }
            finally
            {
                ExitScope();
            }
        }
        if (passedObj is not null && passedObj.Methods.ContainsKey(referencedFunction))
        {
            return InvokeMethod(passedObj, referencedFunction, trueArgs);
        }

        return null;
    }

    // this will only work if the curly brace is on the next line
    private string ParseCodeInCurlyBraces(ref Queue<string> code, ref int lineNumber)
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

        return string.Join('\n', scope);
    }

    private void InitializeFunctions()
    {
        // object[] represents function arguments
        _inbuiltFunctions = new()
        {
            { "print", new Func<object[], object>(StaticInbuilts.Print) },
            { "sleep", new Func<object[], object>(StaticInbuilts.Sleep) },
            { "root", new Func<object[], object>(StaticInbuilts.Root) },
            { "abs", new Func<object[], object>(StaticInbuilts.Abs) },
            { "sin", new Func<object[], object>(StaticInbuilts.Sin) },
            { "cos", new Func<object[], object>(StaticInbuilts.Cos) },
            { "tan", new Func<object[], object>(StaticInbuilts.Tan) },
            { "asin", new Func<object[], object>(StaticInbuilts.Asin) },
            { "acos", new Func<object[], object>(StaticInbuilts.Acos) },
            { "atan", new Func<object[], object>(StaticInbuilts.Atan) },
            { "input", new Func<object[], object>(Input) },
            { "len", new Func<object[], object>(StaticInbuilts.Length) },
            { "stringify", new Func<object[], object>(StaticInbuilts.Stringify) },
            { "randint", new Func<object[], object>(StaticInbuilts.RandomInt)},
            { "ReadFile", new Func<object[], object>(StaticInbuilts.ReadFile) },
            { "WriteFile", new Func<object[], object>(StaticInbuilts.WriteFile) },
            { "deref", new Func<object[], object>(Dereference) }
        };
    }

    private void SkipElseIfElseBlocks(ref Queue<string> codeLines, ref int lineNumber)
    {
        while (codeLines.TryPeek(out string nextLine))
        {
            nextLine = nextLine.Trim();
            if (nextLine.StartsWith("else"))
            {
                codeLines.Dequeue();
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
        while (codeLines.TryDequeue(out string? nextLine))
        {
            nextLine = nextLine.Trim();
            if (nextLine.StartsWith("else if"))
            {
                HandleElseIfBlock(nextLine, ref codeLines, ref lineNumber);
            }
            else if (nextLine.StartsWith("else"))
            {
                HandleElseBlock(ref codeLines, ref lineNumber);
                return;
            }
            else
            {
                codeLines = new Queue<string>(new[] { nextLine }.Concat(codeLines));
                break;
            }
        }
    }

    private void HandleElseIfBlock(string elseIfLine, ref Queue<string> codeLines, ref int lineNumber)
    {
        string elseIfInstruction = elseIfLine[7..].Trim();
        if (!TryParseExpression(elseIfInstruction, out string expression))
        {
            throw new Exception($"Error parsing braces for else if statement: '{elseIfInstruction}'.");
        }

        string scope = ParseCodeInCurlyBraces(ref codeLines, ref lineNumber);
        if (!TryEvaluateBooleanExpression(expression, out bool evaluatedExpression))
        {
            throw new Exception($"Invalid boolean expression for else if statement: '{elseIfInstruction}'.");
        }

        if (!evaluatedExpression) { return; }
        ExecuteScope(scope);
        SkipElseIfElseBlocks(ref codeLines, ref lineNumber);
    }

    private void HandleElseBlock(ref Queue<string> codeLines, ref int lineNumber)
    {
        string scope = ParseCodeInCurlyBraces(ref codeLines, ref lineNumber);
        ExecuteScope(scope);
    }

    private static bool TryParseExpression(string instruction, out string expression)
    {
        if (instruction.Contains('(') && instruction.Contains(')'))
        {
            expression = instruction[1..^1];
            return true;
        }
        expression = string.Empty;
        return false;
    }

    private void ExecuteScope(string scope)
    {
        string error = Interpret(scope);
        if (error != "Code executed successfully.")
        {
            throw new Exception(error);
        }
    }

    private bool TryEvaluateBooleanExpression(string expression, out bool result)
    {
        bool? evaluatedExpression = EvaluateBooleanExpression(expression);
        if (evaluatedExpression.HasValue)
        {
            result = evaluatedExpression.Value;
            return true;
        }
        result = false;
        return false;
    }

    public string Interpret(string code, bool isFromUserFunction = false, DynamicClassInstance? passedObj = null)
    {
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
                string scope;
                string[] args;
                string expression;
                switch (instruction)
                {
                    // class declaration
                    // oh boy (i'm tired)
                    case "class":
                        string classNameAndArgs = string.Join(' ', tokens);
                        string className = classNameAndArgs[..classNameAndArgs.IndexOf('(')];
                        string[] classArgs = classNameAndArgs[(classNameAndArgs.IndexOf('(') + 1)..classNameAndArgs.IndexOf(')')].Split(',').Select(arg => arg.Trim()).ToArray();

                        Queue<string> classScope = new(ParseCodeInCurlyBraces(ref codeLines, ref lineNumber).Split('\n'));

                        ClassDefinition classDef = new(classArgs);
                        
                        while (classScope.Count > 0)
                        {
                            string classLine = classScope.Dequeue().Trim();

                            if (string.IsNullOrWhiteSpace(classLine)) { continue; }

                            Queue<string> classTokens = new(classLine.Split(' '));
                            string classInstruction = classTokens.Dequeue();

                            switch (classInstruction)
                            {
                                case "var":
                                    string propName = classTokens.Dequeue();
                                    classTokens.Dequeue(); // skip the '='
                                    string propValue = string.Join(' ', classTokens);

                                    object? truePropValue;
                                    if (classArgs.Contains(propValue))
                                    {
                                        truePropValue = propValue;
                                    }

                                    else if (!GetValueOfToken(propValue, out truePropValue))
                                    {
                                        throw new Exception($"Error parsing property value for '{propName}' in class '{className}'.");
                                    }

                                    classDef.Properties[propName] = truePropValue;
                                    break;

                                case "fn":
                                    string classMethod = string.Join(' ', classTokens);
                                    string classMethodName = classMethod[..classMethod.IndexOf('(')];
                                    int indexOfOpenBrace = classMethod.IndexOf('(');
                                    string classMethodArgs = classMethod[(indexOfOpenBrace + 1)..^1];
                                    string[] methodArgsArray = classMethodArgs.Split(',').Select(arg => arg.Trim()).ToArray();
                                    string methodCode = ParseCodeInCurlyBraces(ref classScope, ref lineNumber);

                                    classDef.Methods[classMethodName] = (methodArgsArray, methodCode);
                                    break;

                                default:
                                    throw new Exception($"Unrecognized instruction '{classInstruction}' in class '{className}'.");
                            }
                        }

                        _userDefinedClasses[className] = classDef;
                        break;
                    
                    // variable creation
                    case "var":
                        string variableName = tokens.Dequeue();
                        tokens.Dequeue();
                        string givenVariableValue = string.Join(' ', tokens);

                        if (!GetValueOfToken(givenVariableValue, out object? trueVariableValue, passedObj))
                        {
                            throw new Exception($"Error parsing variable type at line: {lineNumber} for variable: '{variableName}', holding value: '{givenVariableValue}'.");
                        }

                        SetVariable(variableName, trueVariableValue);
                        break;

                    case not null when TryGetVariable(instruction, out _):
                        tokens.Dequeue(); // Skip the '='
                        givenVariableValue = string.Join(' ', tokens);

                        if (!GetValueOfToken(givenVariableValue, out trueVariableValue, passedObj))
                        {
                            throw new Exception($"Error parsing variable type at line: {lineNumber} for variable: '{instruction}', holding value: '{givenVariableValue}'.");
                        }

                        if (passedObj is not null && passedObj.Properties.ContainsKey(instruction)) { StaticHelpers.SetProperty(passedObj, instruction, trueVariableValue); }
                        else { SetVariable(instruction, trueVariableValue); }
                        break;

                    // assignment to array or dictionary at index / key
                    case not null when instruction.Contains('[') && instruction.EndsWith(']'):
                        // remove '='
                        tokens.Dequeue();

                        variableName = instruction[..instruction.IndexOf('[')];
                        if (!TryGetVariable(variableName, out object? value)) throw new Exception($"Error at line: {lineNumber}, variable '{variableName}' does not exist.");

                        string attemptedIndex = instruction[(instruction.IndexOf('[') + 1)..instruction.IndexOf(']')];

                        if (!GetValueOfToken(attemptedIndex, out object? index)) { throw new Exception($"Error at line: {lineNumber}, value '{attemptedIndex}' cannot be parsed."); }
                        if (!GetValueOfToken(string.Join(' ', tokens), out value)) { throw new Exception($"Error at line: {lineNumber}, value '{tokens.Dequeue()}' could not be parsed."); }

                        if (TryGetVariable(variableName, out object? variable) && variable is List<object> list)
                        {
                            if (!int.TryParse(index.ToString(), out int intIndex)) { throw new Exception($"Error at line: {lineNumber}, value '{index}' is not an integer."); }

                            if (intIndex < list.Count)
                            {
                                list[intIndex] = value;
                            }
                            else
                            {
                                list.Add(value);
                            }
                            
                            SetVariable(variableName, list);
                        }
                        else if (variable is Dictionary<object, object> dict)
                        {
                            dict[index] = value;
                            SetVariable(variableName, dict);
                        }
                     
                        break;

                    // if statement
                    case "if":
                        string ifStatementInstruction = string.Join(' ', tokens);

                        if (!ifStatementInstruction.Contains('(') || !ifStatementInstruction.Contains(')'))
                            throw new Exception(
                                $"Error parsing braces at line: {lineNumber} for if statement: '{instruction}'.");

                        // get string expression within braces
                        expression = ifStatementInstruction[1..^1];
                        scope = ParseCodeInCurlyBraces(ref codeLines, ref lineNumber);

                        bool? evaluatedExpression = EvaluateBooleanExpression(expression) ?? throw new Exception($"Invalid boolean expression at line: {lineNumber} for if statement: '{instruction}'.");
                        if ((bool)evaluatedExpression)
                        {
                            EnterScope();
                            Interpret(scope, passedObj: passedObj);
                            ExitScope();
                            // skip any following else if or else blocks
                            SkipElseIfElseBlocks(ref codeLines, ref lineNumber);
                        }
                        else
                        {
                            // check for else if or else
                            HandleElseIfElseBlocks(ref codeLines, ref lineNumber);
                        }

                        break;

                    // while loop
                    case "while":
                        string whileLoopInstruction = string.Join(' ', tokens);

                        if (!whileLoopInstruction.Contains('(') || !whileLoopInstruction.EndsWith(')'))
                            throw new Exception(
                                $"Error parsing braces at line: {lineNumber} for while loop: '{instruction}'.");

                        // get string expression within braces
                        expression = whileLoopInstruction[1..^1];
                        scope = ParseCodeInCurlyBraces(ref codeLines, ref lineNumber);

                        if (EvaluateBooleanExpression(expression) is null)
                        {
                            throw new Exception($"Invalid boolean expression at line: {lineNumber} for while loop: '{instruction}'.");
                        }

                        EnterScope();
                        // we have to evaluate it again every time the loop is called to account for updating variables
                        while ((bool)EvaluateBooleanExpression(expression))
                        {
                            Interpret(scope, passedObj: passedObj);
                        }
                        ExitScope();

                        break;

                    // for loop
                    case "for":
                        string forLoopInstruction = string.Join(' ', tokens);

                        if (!(forLoopInstruction.Contains('(') && forLoopInstruction.Contains(')')))
                        { throw new Exception($"Error parsing braces at line: {lineNumber} during for loop: '{instruction}'."); }
                        
                        scope = ParseCodeInCurlyBraces(ref codeLines, ref lineNumber);
                        args = forLoopInstruction[1..^1].Split(';');

                        string[] forLoopVar = args[0].Trim().Split(' ');
                        string forLoopVarName = forLoopVar[0];

                        if (!GetValueOfToken(forLoopVar[2], out object start))
                        {
                            throw new Exception($"Error parsing for loop arguments at line {lineNumber}.");
                        }

                        string[] operation = args[2].Trim().Split(' ');
                        GetValueOfToken(operation[2], out object? operandObject);
                        int operand = Convert.ToInt32(operandObject);
                        string opCode = operation[1];
                        
                        string[] forLoopConditions = args[1].Trim().Split(' ');
                        string condition = forLoopConditions[1];
                        GetValueOfToken(forLoopConditions[2], out object? conditionObject);
                        double conditionValue = Convert.ToDouble(conditionObject);
                        
                        int current = Convert.ToInt32(start);
                        Func<int, int> op = opCode switch
                        {
                            "+" => i => i + operand,
                            "-" => i => i - operand,
                            "*" => i => i * operand,
                            "/" => i => i / operand,
                            _ => throw new Exception($"Invalid operation {opCode}")
                        };

                        Func<int, bool> cond = condition switch
                        {
                            "<" => i => i < conditionValue,
                            "<=" => i => i <= conditionValue,
                            ">" => i => i > conditionValue,
                            ">=" => i => i >= conditionValue,
                            "==" => i => Math.Abs(i - conditionValue) < double.Epsilon,
                            "!=" => i => Math.Abs(i - conditionValue) >= double.Epsilon,
                            _ => throw new Exception($"Invalid condition {condition}")
                        };

                        EnterScope();
                        for (; cond(current); current = op(current))
                        {
                            SetVariable(forLoopVarName, current);
                            Interpret(scope, passedObj: passedObj);
                        }
                        ExitScope();
                        
                        break;

                    // user defined function
                    case "fn":
                        string functionInstruction = string.Join(' ', tokens);

                        if (functionInstruction.Contains('(') && functionInstruction.Contains(')'))
                        {
                            int indexOfOpenBrace = functionInstruction.IndexOf('(');

                            string functionName = functionInstruction[..indexOfOpenBrace];
                            // get array of function arguments
                            args = functionInstruction[(indexOfOpenBrace + 1)..^1].Split(',').Select(arg => arg.Trim()).ToArray();
                            string functionCode = ParseCodeInCurlyBraces(ref codeLines, ref lineNumber);

                            _userDefinedFunctions[functionName] = (args, functionCode);
                            break;
                        }

                        throw new Exception($"Error parsing braces at line: {lineNumber} during function creation: '{instruction}'.");

                    // returning a value from a user defined function
                    case "return":
                        if (isFromUserFunction) { return string.Join(' ', tokens); }

                        throw new Exception($"An error has occurred at line: {lineNumber}. The 'return' keyword can only be used from within a function.");

                    case { } method when method.Contains('.'):
                        try
                        {
                            method = string.Join(' ', line);

                            variableName = method[..method.IndexOf('.')];
                            string methodNameAndArgs = method[(method.IndexOf('.') + 1)..];
                            string methodName = methodNameAndArgs[..methodNameAndArgs.IndexOf('(')];

                            string[] methodArgs = StaticHelpers.ParseArguments(methodNameAndArgs[(methodNameAndArgs.IndexOf('(') + 1)..^1]);

                            if (TryGetVariable(variableName, out object? potentialObject) && potentialObject is DynamicClassInstance instance)
                            {
                                object?[] trueArgs = new object?[methodArgs.Length];

                                if (methodArgs.Where((t, i) => !GetValueOfToken(t, out trueArgs[i])).Any())
                                {
                                    throw new Exception($"An error has occurred at line: {lineNumber}. The method '{method}' could not be found.");
                                }

                                InvokeMethod(instance, methodName, trueArgs);
                            }

                            else if (!TryExecuteMethod(variableName, methodName, methodArgs, out object? _))
                            {
                                throw new Exception($"An error has occurred at line: {lineNumber}. The method '{method}' could not be found.");
                            }
                        }
                        catch
                        {
                            tokens.Dequeue(); // skip '='

                            givenVariableValue = tokens.Dequeue();

                            // potentially a naive approach
                            string classAndMethod = method[..method.IndexOf(' ')];

                            // handle setting a property on an object
                            string objectName = classAndMethod[..classAndMethod.IndexOf('.')];
                            string propertyName = classAndMethod[(classAndMethod.IndexOf('.') + 1)..];

                            if (!TryGetVariable(objectName, out object? obj) || obj is not DynamicClassInstance pyObj)
                            {
                                throw new Exception($"Variable '{objectName}' is not an object.");
                            }

                            if (!GetValueOfToken(givenVariableValue, out trueVariableValue))
                            {
                                throw new Exception($"Error parsing value '{givenVariableValue}' for property '{propertyName}' on object '{objectName}'.");
                            }

                            StaticHelpers.SetProperty(pyObj, propertyName, trueVariableValue);
                        }
                        
                        break;

                    // if all else fails, we attempt to see if the user is referencing a function
                    default:
                        if (!(line.Contains('(') && line.Contains(')'))) 
                        { throw new Exception($"Execution failed at line {lineNumber}: Unrecognized instruction '{instruction}'."); }
                        
                        // a return type of null signifies that something went wrong with processing
                        if (GetValueFromFunction(line, passedObj) is null)
                        {
                            throw new Exception($"Error parsing function name or arguments at line {lineNumber}.");
                        }

                        break;
                }
            }
            catch (Exception ex)
            {
                throw new Exception($"Error at line {lineNumber}: {ex.Message}", ex);
            }
        }

        return "Code executed successfully.";
    }

    [GeneratedRegex(@"\{(\w+)\}")]
    private static partial Regex MyRegex();
}

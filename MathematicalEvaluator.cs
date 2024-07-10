using System.Text.RegularExpressions;

namespace pysharp;
public static partial class MathematicalEvaluator
{
    private static readonly Dictionary<string, (int Precedence, bool RightAssociative)> Operators = new()
    {
        {"+", (1, false)}, // add
        {"-", (1, false)}, // subtract
        {"*", (2, false)}, // multiply
        {"/", (2, false)}, // divide
        {"%", (2, false)}, // mod
        {"**", (3, false)}, // power
        {"u-", (4, true)}  // unary minus
    };

    public static double Evaluate(string expression, Dictionary<string, object?> variables)
    {
        List<string> tokens = Tokenize(expression);
        Queue<string> rpn = ShuntingYard(tokens);
        return EvaluateRPN(rpn, variables);
    }

    private static List<string> Tokenize(string expression)
    {
        List<string> tokens = new();
        // ^[a-zA-Z_]\w*$
        Regex regex = MyRegex();
        var matches = regex.Matches(expression);
        
        // this will control whether shunting yard will interpret a minus as an operator or a number
        bool expectOperand = true;
        
        // ^[a-zA-Z_]\w*$ checks for variables, and allows them to be evaluated
        foreach (Match match in matches)
        {
            string token = match.Value;
            if (double.TryParse(token, out _) || regex.IsMatch(token))
            {
                tokens.Add(token);
                expectOperand = false;
            }
            // 'u-' is unary minus (not operator)
            else if (token == "-" && expectOperand)
            {
                tokens.Add("u-");
            }
            else
            {
                tokens.Add(token);
                expectOperand = token != ")";
            }
        }

        return tokens;
    }

    private static Queue<string> ShuntingYard(List<string> tokens)
    {
        Queue<string> output = new();
        Stack<string> operators = new();

        foreach (string token in tokens)
        {
            if (double.TryParse(token, out _) || MyRegex1().IsMatch(token))
            {
                output.Enqueue(token);
            }
            else switch (token)
            {
                // evaluate tokens within brackets as higher priority
                case "(":
                    operators.Push(token);
                    break;
                case ")":
                {
                    while (operators.Count > 0 && operators.Peek() != "(")
                    {
                        output.Enqueue(operators.Pop());
                    }
                    if (operators.Count > 0 && operators.Peek() == "(")
                    {
                        operators.Pop();
                    }

                    break;
                }
                default:
                {
                    if (Operators.TryGetValue(token, out (int Precedence, bool RightAssociative) value))
                    {
                        // pop each token and compute it with its respective operator
                        while (operators.Count > 0 && operators.Peek() != "(" &&
                               (Operators[operators.Peek()].Precedence > value.Precedence ||
                                (Operators[operators.Peek()].Precedence == value.Precedence && !value.RightAssociative)))
                        {
                            output.Enqueue(operators.Pop());
                        }
                        operators.Push(token);
                    }

                    break;
                }
            }
        }

        while (operators.Count > 0)
        {
            output.Enqueue(operators.Pop());
        }

        return output;
    }

    private static double EvaluateRPN(Queue<string> rpn, Dictionary<string, object?> variables)
    {
        Stack<object> stack = new();

        while (rpn.Count > 0)
        {
            string token = rpn.Dequeue();

            // attempt to either get the exact numerical equivalent
            // if that doesn't work, check to see if user is referencing a known variable
            if (double.TryParse(token, out double number))
            {
                stack.Push(number);
            }
            else if (variables.TryGetValue(token, out object? variable))
            {
                stack.Push(variable);
            }
            else if (Operators.ContainsKey(token))
            {
                // if the unary minus is encountered (as inferred from Tokenize()) instead replace it with the value's -
                // - negative equivalent
                if (token == "u-")
                {
                    object operand = stack.Pop();
                    stack.Push(-Convert.ToDouble(operand));
                }
                else
                {
                    object b = stack.Pop();
                    object a = stack.Pop();
                    // apply correct operation
                    switch (token)
                    {
                        case "+": stack.Push(Convert.ToDouble(a) + Convert.ToDouble(b)); break; // add
                        case "-": stack.Push(Convert.ToDouble(a) - Convert.ToDouble(b)); break; // subtract
                        case "*": stack.Push(Convert.ToDouble(a) * Convert.ToDouble(b)); break; // multiply
                        case "/": stack.Push(Convert.ToDouble(a) / Convert.ToDouble(b)); break; // divide
                        case "%": stack.Push(Convert.ToDouble(a) % Convert.ToDouble(b)); break; // mod
                        case "**": stack.Push(Math.Pow(Convert.ToDouble(a), Convert.ToDouble(b))); break; // power
                    }
                }
            }
            else
            {
                throw new ArgumentException($"Unknown token: {token}");
            }
        }

        return Convert.ToDouble(stack.Pop());
    }

    // checks if expression is valid
    [GeneratedRegex(@"(\d*\.?\d+)|([a-zA-Z_]\w*)|(\*\*|[+\-*/()%^])")]
    private static partial Regex MyRegex();
    
    // matches variable paradigm
    [GeneratedRegex(@"^[a-zA-Z_]\w*$")]
    private static partial Regex MyRegex1();
}
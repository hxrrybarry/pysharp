using System.Text.RegularExpressions;

namespace pysharp;
public static partial class MathematicalEvaluator
{
    private static readonly Dictionary<string, (int Precedence, bool RightAssociative)> Operators = new()
    {
        {"+", (1, false)},
        {"-", (1, false)},
        {"*", (2, false)},
        {"/", (2, false)},
        {"u-", (4, true)}  // unary minus
    };

    public static double Evaluate(string expression, Dictionary<string, object?> variables)
    {
        var tokens = Tokenize(expression);
        var rpn = ShuntingYard(tokens);
        return EvaluateRPN(rpn, variables);
    }

    private static List<string> Tokenize(string expression)
    {
        List<string> tokens = new();
        Regex regex = MyRegex();
        var matches = regex.Matches(expression);

        bool expectOperand = true;

        foreach (Match match in matches)
        {
            string token = match.Value;
            if (double.TryParse(token, out _) || MyRegex1().IsMatch(token))
            {
                tokens.Add(token);
                expectOperand = false;
            }
            else if (token == "-" && expectOperand)
            {
                tokens.Add("u-");
            }
            else
            {
                tokens.Add(token);
                expectOperand = (token != ")");
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
            if (double.TryParse(token, out _) || MyRegex2().IsMatch(token))
            {
                output.Enqueue(token);
            }
            else switch (token)
            {
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
                if (token == "u-")
                {
                    object operand = stack.Pop();
                    stack.Push(-Convert.ToDouble(operand));
                }
                else
                {
                    object b = stack.Pop();
                    object a = stack.Pop();
                    switch (token)
                    {
                        case "+": stack.Push(Convert.ToDouble(a) + Convert.ToDouble(b)); break;
                        case "-": stack.Push(Convert.ToDouble(a) - Convert.ToDouble(b)); break;
                        case "*": stack.Push(Convert.ToDouble(a) * Convert.ToDouble(b)); break;
                        case "/": stack.Push(Convert.ToDouble(a) / Convert.ToDouble(b)); break;
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

    [GeneratedRegex(@"(\d*\.?\d+)|([a-zA-Z_]\w*)|([+\-*/()])")]
    private static partial Regex MyRegex();
    [GeneratedRegex(@"^[a-zA-Z_]\w*$")]
    private static partial Regex MyRegex1();
    [GeneratedRegex(@"^[a-zA-Z_]\w*$")]
    private static partial Regex MyRegex2();
}
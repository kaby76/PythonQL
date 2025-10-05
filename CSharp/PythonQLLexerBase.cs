using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using Antlr4.Runtime;

public abstract class PythonQLLexerBase : Lexer
{
	public PythonQLLexerBase(ICharStream input) : base(input)
	{
	}

	public PythonQLLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput) : base(input, output, errorOutput)
	{
	}

	private LinkedList<IToken> Tokens = new LinkedList<IToken>();
	private Stack<int> Indents = new Stack<int>();
	private int Opened = 0;
	private IToken LastToken = null;

	private CommonToken CommonToken(int type, string text)
	{
		int stop = CharIndex - 1;
		int start = text.Length == 0 ? stop : stop - text.Length + 1;
		return new CommonToken(Tuple.Create((ITokenSource)this, (ICharStream)InputStream), type, DefaultTokenChannel, start, stop);
	}


	public override void Emit(IToken token)
	{
		base.Token = token;
		Tokens.AddLast(token);
	}

	public void PlusOpened()
	{
		this.Opened++;
	}

	public void MinusOpened()
	{
		this.Opened--;
	}

	public bool AtStartOfInput() => this.Column == 0 && this.Line == 1;

	public void NewLineAction()
	{
		var newLine = (new Regex("[^\r\n\f]+")).Replace(Text, "");
		var spaces = (new Regex("[\r\n\f]+")).Replace(Text, "");

		// Strip newlines inside open clauses except if we are near EOF. We keep NEWLINEs near EOF to
		// satisfy the final newline needed by the single_put rule used by the REPL.
		int next = ((ICharStream)InputStream).LA(1);
		int nextnext = ((ICharStream)InputStream).LA(2);
		if (Opened > 0 || (nextnext != -1 && (next == '\r' || next == '\n' || next == '\f' || next == '#')))
		{
			// If we're inside a list or on a blank line, ignore all indents, 
			// dedents and line breaks.
			Skip();
		}
		else
		{
			if (LastToken != null)
			{
				Emit(CommonToken(PythonQLParser.NEWLINE, newLine));
			}
			int indent = GetIndentationCount(spaces);
			int previous = Indents.Count == 0 ? 0 : Indents.Peek();
			if (indent == previous)
			{
				// skip indents of the same size as the present indent-size
				Skip();
			}
			else if (indent > previous) {
				Indents.Push(indent);
				Emit(CommonToken(PythonQLParser.INDENT, spaces));
			}
			else {
				// Possibly emit more than 1 DEDENT token.
				while(Indents.Count != 0 && Indents.Peek() > indent)
				{
					//this.Emit(CreateDedent());
					Indents.Pop();
				}
			}
		}
	}

	public override void Reset()
	{
		Tokens = new LinkedList<IToken>();
		Indents = new Stack<int>();
		Opened = 0;
		LastToken = null;
		base.Reset();
	}

	public override IToken NextToken()
	{
		// Check if the end-of-file is ahead and there are still some DEDENTS expected.
		if (((ICharStream)InputStream).LA(1) == Eof && Indents.Count != 0)
		{
			// Remove any trailing EOF tokens from our buffer.
			for (var node  = Tokens.First; node != null; )
			{
				var temp = node.Next;
				if (node.Value.Type == Eof)
				{
					Tokens.Remove(node);
				}
				node = temp;
			}

			// First emit an extra line break that serves as the end of the statement.
			this.Emit(CommonToken(PythonQLParser.NEWLINE, "\n"));

			// Now emit as much DEDENT tokens as needed.
			while (Indents.Count != 0)
			{
//				Emit(CreateDedent());
				Indents.Pop();
			}

			// Put the EOF back on the token stream.
			Emit(CommonToken(PythonQLParser.Eof, "<EOF>"));
		}

		var next = base.NextToken();
		if (next.Channel == DefaultTokenChannel)
		{
			// Keep track of the last token on the default channel.
			LastToken = next;
		}

		if (Tokens.Count == 0)
		{
			return next;
		}
		else
		{
			var x = Tokens.First.Value;
			Tokens.RemoveFirst();
			return x;
		}
	}

	int GetIndentationCount(string spaces)
	{
		int count = 0;
		foreach (char ch in spaces.ToCharArray())
		{
			count += ch == '\t' ? 8 - (count % 8) : 1;
		}
		return count;
	}
}

# Noah Mizell and Venu Kunche
# 24FEB2018
# Lexer Program that works with the given BNF grammar
# for a the SimPL language.

# This program cannot handle code that contains arguments
# for the SimPL language.
# This should be a simple fix.


class Lexer

    def initialize(file)
        @i = 0
        @program = File.open(file, 'r', &:read)
            .split(/\b/)
            .map { |s| s.tr(' ', '').tr("\n", '').tr("\t", '') }
            .reject { |s| s.empty? }
        stmts()
    end

    def stmts()
        if (@program[@i] == "for") || (@program[@i] == "if") || (@program[@i+1] == ":=")
            if @program[@i] != ";"
                stmt()
            end
            if @program[@i].include? ";"
                @i += 1
            else
                puts(@program[@i])
                puts(@program[@i+1])
                puts(@program[@i+2])
                puts(@i)
                puts(@program.size)
                puts("Error: no semicolon")
            end
            stmts()
        end
    end

    def stmt()
        if @program[@i] == "if"
            lexpr()
            @i += 1
            if @program[@i] == "then" 
                @program[@i] == ""
            else
                puts("Error: should be then")
            end
            @i += 1
            stmts()
            if(@program[@i].include? "else")
                stmts()
            end
            if @program[@i] == "end"
                @program[@i] = ""
                @i += 1
            else
                puts("Error: no end to if")
            end
        end
        if @program[@i] == "for"
            @i = @i + 2
            if @program[@i] == "from"
                @i += 1
            else
                puts("Error: should be for")
            end
            addop()
            if @program[@i] == "to"
                @i += 1
            else
                puts("Error: should be to")
            end
            addop()
            if (@program[@i].include? "do") || (@program[@i] == "by")
                if @program[@i].include? "do"
                    @i += 1
                    stmts()
                end
                if @program[@i] == "by"
                    @i += 1
                    addop()
                    if @program[@i].include? "do"
                        @i += 1
                        stmts()
                    else
                        puts("Error: should be do")
                    end
                end
            else
                puts("Error: should be do or by")
            end
            if @program[@i] == "end"
                @program[@i] = ""
                @i += 1
            else
                puts("Error: should be end")
            end
        end
        if @program[@i+1] == ":="
            @i = @i + 2
            addop()
        end
    end

    def addop()
        mulop()
        if @program[@i] == "+" || @program[@i] == "-"
            @program[@i] = ""
            @i += 1
            addop()
        end
    end

    def mulop()
        factor()
        if @program[@i] == "*" || @program[@i] == "/"
            @program[@i] = ""
            @i += 1
            mulop()
        end
    end

    def factor()
        if @program[@i].include? "("
            @i += 1
            addop()    
            if @program[@i].include? ")"
                @program[@i] = ""
            else
                puts("Error: no )")
            end
        end
        @i += 1
    end

    def lexpr()
        lterm()
        if @program[@i] == "and"
            @program[@i] = ""
            @i += 1
            lexpr()
        end
    end

    def lterm()
        if @program[@i] == "not"
            @program[@i] = ""
            @i += 1
            lfactor()
        else
            lfactor()
        end
    end

    def lfactor()
        if @program[@i] != "true" && @program[@i] != "false"
            relop()
        end
        @i += 1
    end

    def relop()
        addop()
        if (@program[@i]=="<=") || (@program[@i]=="<") || (@program[@i]=="=")
            @i += 1
            addop()
        else
            puts("Error: no comparitive operator")
        end
    end
end

Lexer.new("rubytest.txt")
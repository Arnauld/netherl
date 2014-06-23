require 'rake/clean'

INCLUDE = "include"
SRC = FileList['src/*.erl']
CLEAN.include("ebin/*.beam")

task :default => [:compile, :test]

desc "Compiles all .erl files in src to .beam files in ebin"
task :compile do
  sh %{./rebar get-deps compile}
end

desc "Run all tests"
task :test do
  rm_rf '.eunit'
  mkdir '.eunit'
  output = `./rebar eunit skip_deps=true`

  output.each_line do |line|
    case line
    when /= (EUnit) =/
      print line.gsub($1, green($1))
    when /\*failed\*/
      print red(line)
    when /(\.\.\..*ok)/
      print line.gsub($1,green($1))
    when /Failed:\s+(\d+)\.\s+Skipped:\s+(\d+)\.\s+Passed:\s+(\d+)\./
      puts "#{red("Failed: #{$1}")}, Skipped: #{$2}, #{green("Passed: #{$3}")}"
      `say #{$3} passed but #{$1} tests failed`
    when/(All \d+ tests passed.)/
      print green(line)
      `say #{line}`
    else
      print line
    end
  end
end

def green(text)
  "\e[32m#{text}\e[0m"
end

def red(text)
  "\e[31m#{text}\e[0m"
end
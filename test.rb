RUNS = 1000

turns  = []
scores = []
wins   = 0

RUNS.times do
  states = `./2048`.split(/\n/)
  result = states.pop

  turns  << states.length
  scores << result.scan(/\d+/).map(&:to_i).inject(:+)
  wins += 1 if result =~ /Victory/
end

puts "AVG. TURNS: #{ turns.inject(:+) / RUNS }"
puts "AVG. SCORE: #{ scores.inject(:+) / RUNS }"
puts "WINS:       #{ wins }"

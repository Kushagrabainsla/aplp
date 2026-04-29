#! /usr/bin/ruby -w

# Represents a Rogerian psychiatrist
class Shrink

  # initializes 'memory' of Eliza.
  def initialize()
    @he="he"
    @she="she"
    @they="they"
  end

  # read a statement and convert it to a psychiatric response.
  def generateResponse(blather)
    blather = filter_filler_words(blather.downcase.strip)
    
    if blather =~ /\b(always|never)\b/i
      return "CAN YOU BE MORE SPECIFIC?"
    end

    if blather =~ /^are you\b(.*?)\??$/i
      rest_of_statement = transpose_pronouns($1.clone)
      return "IS IT IMPORTANT IF I AM#{rest_of_statement.upcase}?"
    end
    
    # Custom addition to make Eliza more human
    if blather =~ /\bi feel\b(.*?)\??$/i
      rest_of_statement = transpose_pronouns($1.clone)
      return "WHY DO YOU FEEL#{rest_of_statement.upcase}?"
    end

    blather = transpose_pronouns(blather)
    blather = substitute_past_references(blather)
    update_future_references(blather)
    blather = handle_name_introduction(blather)

    format_response(blather)
  end

  private

  def filter_filler_words(statement)
    statement.sub(/^(well|perhaps)[,\s]*/i, '')
  end

  def transpose_pronouns(statement)
    # Temporary uppercase to avoid re-replacement collisions
    statement = statement.gsub(/\byour\b/i, "MY")
    statement.gsub!(/\byou\b/i, 'I')

    statement.gsub!(/\bmy\b/i, "your")
    statement.gsub!(/\bme\b/i, "you")
    statement.gsub!(/\bi\b/i, 'you')
    
    statement
  end

  def substitute_past_references(statement)
    statement = statement.sub(/\b(he|him)\b/i, @he)
    statement = statement.sub(/\b(she|her)\b/i, @she)
    statement = statement.sub(/\b(they|them)\b/i, @they)
    statement
  end

  def update_future_references(statement)
    hePat = /.*\b(your (father|brother|(ex-?)?(husband|boyfriend)))\b.*/i
    shePat = /.*\b(your (mother|sister|(ex-?)?(wife|girlfriend)))\b.*/i
    theyPat = /.*\b(your (parents|friends|siblings|children))\b.*/i

    @he = statement.sub(hePat, '\1').chomp if statement =~ hePat 
    @she = statement.sub(shePat, '\1').chomp if statement =~ shePat
    @they = statement.sub(theyPat, '\1').chomp if statement =~ theyPat
  end

  def handle_name_introduction(statement)
    namePat = /.*\byour name is (\w+).*/i
    if statement =~ namePat
      @name = statement.sub(namePat, '\1')
      statement.sub!(namePat, 'nice to meet you, \1.  How can I help you')
    end
    statement
  end

  def format_response(statement)
    statement.sub!(/\??$/, '?') # Ensure exactly one question mark at the end
    statement.upcase
  end
end

#main -- reads from standard input unless -test is the first parameter.
eliza = Shrink.new()
if ARGV[0] == "-test"
    ['My girlfriend never listens to me',
     "I think she might be deaf",
     "yes",
     "I am afraid of clowns",
     "Well, they just seem creepy",
     "Also, when I was a kid, a clown killed my dad",
     "Are you a clown in disguise?",
     "Perhaps they are coming for me",
     "I feel you ignore me"
    ].each do |stmt|
        puts stmt
        puts eliza.generateResponse(stmt)
        puts "-" * 20
    end
else
  while line = gets
    response = eliza.generateResponse(line)
    puts response
  end
end

#
# ruby benchmark/goliath.rb -sv
#
# http://localhost:9000/

require 'goliath'

puts 'Running at http://localhost:9000/'

class Hello < Goliath::API
  def response(env)
    [200, {}, "Hello, World"]
  end
end

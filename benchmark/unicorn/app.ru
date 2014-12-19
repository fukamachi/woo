# -*- mode: ruby -*-
#
# unicorn -E production -c benchmark/unicorn/config.rb benchmark/unicorn/app.ru
#
# http://localhost:8080/

class HelloApp
  def call(env)
    [ 
      200,
      {},
      ['Hello, World']
    ]
  end
end
run HelloApp.new

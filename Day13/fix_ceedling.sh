# See https://github.com/ThrowTheSwitch/Ceedling/issues/615#issuecomment-975287310 for why we patch ceedling after installing  
echo About to...
grep '\(YAML\.load.*result\))$' .devenv/state/.bundle/ruby/3.1.0/gems/ceedling-0.31.1/lib/ceedling/yaml_wrapper.rb
grep '\(YAML\.load.*filepath\))$' .devenv/state/.bundle/ruby/3.1.0/gems/ceedling-0.31.1/bin/ceedling
sed -i 's/\(YAML\.load.*result\))$/\1, aliases:true)/' .devenv/state/.bundle/ruby/3.1.0/gems/ceedling-0.31.1/lib/ceedling/yaml_wrapper.rb
sed -i 's/\(YAML\.load.*filepath\))$/\1, aliases:true)/' .devenv/state/.bundle/ruby/3.1.0/gems/ceedling-0.31.1/bin/ceedling
echo Done.
#require './plugins/raw'
module Jekyll
  class GraphvizBlock < Liquid::Block

    def initialize(tag_name, text, tokens)
      super
    end

    def render(context)
      #output = super
      code = "<div class='hello'>"
      code += super.join
      code += "</div>"
      code
    end
  end
end

Liquid::Template.register_tag('graphviz', Jekyll::GraphvizBlock)

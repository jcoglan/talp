class Pattern
  attr_reader :name, :args

  def initialize(name, args)
    @name = name
    @args = args
  end

  def inspect
    args = @args.empty? ? '' : "(#{@args.map(&:inspect) * ' '})"
    "#{@name}#{args}"
  end

  def ===(value)
    if value.is_a?(Pattern) and @name == value.name and @args.size == value.args.size
      @args.each.with_index.all? do |arg, i|
        arg === value.args[i]
      end
    else
      false
    end
  end

  def self.define(*names)
    names.each do |name|
      Object.const_set(name, Pattern.new(name, []))

      Kernel.module_eval do
        define_method(name) { |*args| Pattern.new(name, args) }
      end
    end
  end

  def self.match(pattern, &block)
    Matcher.new(pattern).run(&block)
    raise NoRuleApplies
  rescue Result => r
    r.value
  end

  class Result < StandardError
    attr_reader :value

    def initialize(value)
      @value = value
    end
  end

  NoRuleApplies = Class.new(StandardError)

  class Matcher
    def initialize(subject)
      @subject = subject
    end

    def method_missing(name)
      if @matched
        @vars[name].value
      else
        @vars ||= {}
        @vars[name] ||= Variable.new(name)
      end
    end

    def run(&block)
      instance_exec(self, &block)
    end

    def on(pattern, result)
      if pattern === @subject
        @matched = true
        value = result.call
        if value.nil?
          @matched = false
        else
          raise Result.new(value)
        end
      end
      @vars = nil
    end
  end

  class Variable
    attr_reader :value

    def initialize(name)
      @name = name
    end

    def inspect
      value = @value ? "{#{@value}}" : ''
      "#{@name}#{value}"
    end

    def ===(value)
      if defined? @value
        @value === value
      else
        @value = value
        true
      end
    end
  end
end

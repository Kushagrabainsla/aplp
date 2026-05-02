class Tree
  attr_accessor :value, :left, :right

  def initialize(value, left = nil, right = nil)
    @value = value
    @left = left
    @right = right
  end

  def each_node(&block)
    yield @value
    @left.each_node(&block) if @left
    @right.each_node(&block) if @right
  end

  def method_missing(method_name, *args, &block)
    path_segments = parse_path(method_name)
    return super unless path_segments

    traverse_path(path_segments)
  end

  private

  def parse_path(method_name)
    segments = method_name.to_s.split('_')
    segments if segments.all? { |segment| valid_direction?(segment) }
  end

  def valid_direction?(segment)
    segment == 'left' || segment == 'right'
  end

  def traverse_path(path_segments)
    current_node = self
    path_segments.each do |direction|
      current_node = current_node.send(direction)
      return nil unless current_node
    end
    current_node.value
  end
end

my_tree = Tree.new(42,
                   Tree.new(3,
                            Tree.new(1,
                                     Tree.new(7,
                                              Tree.new(22),
                                              Tree.new(123)),
                                     Tree.new(32))),
                   Tree.new(99,
                            Tree.new(81)))

my_tree.each_node do |v|
  puts v
end

arr = []
my_tree.each_node do |v|
  arr.push v
end
p arr

p "Getting nodes from tree"
p my_tree.left_left
p my_tree.right_left
p my_tree.left_left_right
p my_tree.left_left_left_right
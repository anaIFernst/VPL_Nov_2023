# Tutorials

This notebook contains several tutorials on the Virtual Plant Laboratory.
The following tutorials are included:

* [Tutorial 01: Algae growth](#Tutorial-01:-Algae-growth)
* [Tutorial 02: Snowflakes](#Tutorial-02:-Snowflakes)
* [Tutorial 03: Single tree](#Tutorial-03:-Single-tree)
* [Tutorial 04: Forest](#Tutorial-04:-Forest)
* [Tutorial 05: Growth-driven forest](#Tutorial-05:-Growth-driven-forest)
* [Tutorial 06: RUE-driven forest](#Tutorial-06:-RUE-driven-forest)
* [Tutorial 07: Canopy photosynthesis](#Tutorial-07:-Canopy-photosynthesis)
* [Tutorial 08: Ground cover calculations](#Tutorial-08:-Ground-cover-calculations)

## Setup

We will need to install VirtualPlantLab.jl and two of the VPLverse packages (Ecophys.jl and SkyDomes.jl). Two packages are needed for the visualization aspects (GLMakie.jl and ColorTypes.jl) and three packages for plotting (Plots.jl), numerical integration (FastGaussQuadrature.jl) and creating distributions (Distributions.jl).

```julia
import Pkg # Julia's package manager
# VPLverse packages
Pkg.add("VirtualPlantLab")
Pkg.add("Ecophys")
Pkg.add("SkyDomes")
Pkg.add("GLMakie")
# Additional packages
for pkg in ["Plots", "Distributions", "FastGaussQuadrature", "ColorTypes"]
    Pkg.add(pkg)
end
```

## Tutorial 01: Algae growth

In this first example, we learn how to create a `Graph` and update it dynamically with rewriting rules.

The model described here is based on the non-branching model of [algae growth](https://en.wikipedia.org/wiki/L-system#Example_1:_Algae) proposed by Lindermayer as one of the first L-systems.

First, we need to load the VirtualPlantLab.jl package:

```julia
using VirtualPlantLab
```

The rewriting rules of the L-system are as follows:

**axiom**:   A

**rule 1**:  A $\rightarrow$ AB

**rule 2**:  B $\rightarrow$ A

In VPL, this L-system would be implemented as a graph where the nodes can be of type `A` or `B` and inherit from the abstract type `Node`. It is advised to include type definitions in a module to avoid having to restart the Julia session whenever we want to redefine them. Because each module is an independent namespace, we need to import `Node` from the VPL package inside the module:

```julia
module algae
    import VirtualPlantLab: Node
    struct A <: Node end
    struct B <: Node end
end
import .algae
```

Note that in this very example we do not need to store any data or state inside the nodes, so types `A` and `B` do not require fields.

The axiom is simply defined as an instance of type of `A`:

```julia
axiom = algae.A()
```

The rewriting rules are implemented in VPL as objects of type `Rule`. In VPL, a rewriting rule substitutes a node in a graph with a new node or subgraph and is therefore composed of two parts:

1. A condition that is tested against each node in a graph to choose which nodes to rewrite.
2. A subgraph that will replace each node selected by the condition above.

In VPL, the condition is split into two components:

1. The type of node to be selected (in this example that would be `A` or `B`).
2. A function that is applied to each node in the graph (of the specified type) to indicate whether the node should be selected or not. This function is optional (the default is to select every node of the specified type).

The replacement subgraph is specified by a function that takes as input the node selected and returns a subgraph defined as a combination of node objects. Subgraphs (which can also be used as axioms) are created by linearly combining objects that inherit from `Node`. The operation `+` implies a linear relationship between two nodes and `()` indicates branching (see later tutorials for examples of branching).

The implementation of the two rules of algae growth model in VPL is as follows:

```julia
rule1 = Rule(algae.A, rhs = x -> algae.A() + algae.B())
rule2 = Rule(algae.B, rhs = x -> algae.A())
```

Note that in each case, the argument `rhs` is being assigned an anonymous function. This is a function without a name that is defined directly inside the function call. That is, the Julia expression `x -> A() + B()` is equivalent to the following function definition:

```julia
function rule_1(x)
    algae.A() + algae.B()
end
```

For simple rules (especially if the right hand side is just a line of code) it is easier to just define the right hand side of the rule with an anonymous function rather than creating a standalone function with a meaningful name.  However, standalone functions are easier to debug as you can call them directly from the REPL.

With the axiom and rules we can now create a `Graph` object that represents the algae organism. The first argument is the axiom and the second is a tuple with all the rewriting rules:

```julia
organism = Graph(axiom = axiom, rules = (rule1, rule2))
```

If we apply the rewriting rules iteratively, the graph will grow, in this case representing the growth of the algae organism. The rewriting rules are applied on the graph with the function `rewrite!()` which modifies the graph in-place (no copy is created, so the original state of the graph is now deleted):

```julia
rewrite!(organism)
```

Since there was only one node of type `A`, the only rule that was applied was `rule1`, so the graph should now have two nodes of types `A` and `B`, respectively. We can confirm this by drawing the graph. We do this with the function `draw()` which will always generate the same representation of the graph, but different options are available depending on the context where the code is executed. To choose a graphical backend we first need to load it. In this case, we will use the native OpenGL backend, so we load GLMakie.jl (see [3D visualization](https://virtualplantlab.com/stable/manual/Visualization/) for more details on the different backends):

```julia
import GLMakie
draw(organism)
```

Notice that each node in the network representation is labelled with the type of node (`A` or `B` in this case) followed by a number. This number is a unique identifier associated to each node, and it is useful for debugging purposes as you can extract nodes by this index. For example, if the first node has is identified by 2, you can extract the data stored in this node with `data(organism[2])`.

Applying multiple iterations of rewriting can be achieved with a simple loop:

```julia
for i in 1:4
    rewrite!(organism)
end
```

Add we can verify that the graph grew as expected:

```julia
draw(organism)
```

The network is rather boring as the system is growing linearly (no branching) but it already illustrates how graphs can grow rapidly in just a few iterations. Remember that the interactive visualization allows adjusting the zoom, which is handy when graphs become large. Also, note that the original nodes were rewritten, which is why the identifiers are now different. There is a global counter for nodes identifiers that is incremented each time a new node is created as part of a rule, so relying on a particular identifier is not recommended since it will depend on the context in which the code is run. You can reset the counter (to gain some reproducibility) with the function `reset_id!()` from the module `PlantGraphs`:

```julia
import PlantGraphs
PlantGraphs.reset_id!()
```

## Tutorial 02: Snowflakes

In this example, we create a Koch snowflake, which is one of the earliest fractals to be described. The Koch snowflake is a closed curve composed on multiple of segments of different lengths. Starting with an equilateral triangle, each segment in the snowflake is replaced by four segments of smaller length arranged in a specific manner. Graphically, the first four iterations of the Koch snowflake construction process result in the following figures (the green segments are shown as guides, but they are not part of the snowflake):

![First four iterations of Koch snowflake fractal](./img/KochWikipedia.png)

In order to implement the construction process of a Koch snowflake in VPL we need to understand how a 3D structure can be generated from a graph of nodes. VPL uses a procedural approach to generate of structure based on the concept of turtle graphics.

The idea behind this approach is to imagine a turtle located in space with a particular position and orientation. The turtle then starts consuming the different nodes in the graph (following its topological structure) and generates 3D structures as defined by the user for each type of node. The consumption of a node may also include instructions to move and/or rotate the turtle, which allows to alter the relative position of the different 3D structures described by a graph.

The construction process of the Koch snowflake in VPL could then be represented by the following axiom and rewriting rule:

**axiom**: E(L) + RU(120) + E(L) + RU(120) + E(L)
**rule**: E(L) → E(L/3) + RU(-60) + E(L/3) + RU(120) + E(L/3) + RU(-60) + E(L/3)

Where E represent an edge of a given length (`L` ir `L/3`) and RU represents a rotation of the turtle around the upward axis, with the angle of rotation in hexadecimal degrees. The rule can be visualized as follows:

![Koch construction rule](./img/Koch_order_1.png)

Note that VPL already provides several classes for common turtle movements and rotations, so our implementation of the Koch snowflake only needs to define a class to implement the edges of the snowflake. This can be achieved as follows:

```julia
using VirtualPlantLab
import ColorTypes: RGB
module sn
    import VirtualPlantLab: Node
    struct E <: Node
        length::Float64
    end
end
import .sn
```

Note that nodes of type E need to keep track of the length as illustrated in the above. The axiom is straightforward:

```julia
const L = 1.0
axiom = sn.E(L) + RU(120.0) + sn.E(L) + RU(120.0) + sn.E(L)
```

The rule is also straightforward to implement as all the nodes of type E will be replaced in each iteration. However, we need to ensure that the length of the new edges is a calculated from the length of the edge being replaced. In order to extract the data stored in the node being replaced we can simply use the function `data()`. In this case, the replacement function is defined and then added to the rule. This can make the code more readable and helps debugging and testing the replacement function.

```julia
function Kochsnowflake(x)
    L = data(x).length
    sn.E(L/3) + RU(-60.0) + sn.E(L/3) + RU(120.0) + sn.E(L/3) + RU(-60.0) + sn.E(L/3)
 end
 rule = Rule(sn.E, rhs = Kochsnowflake)
```

The model is then created by constructing the graph from the axiom and rule:

```julia
Koch = Graph(axiom = axiom, rules = rule)
```

In order to be able to generate a 3D structure we need to define a method for the function `VirtualPlantLab.feed!` (notice the need to prefix it with `VirtualPlantLab.` as we are going to define a method for this function). The method needs to take two arguments, the first one is always an object of type `Turtle` and the second is an object of the type for which the method is defined (in this case, `E`):

```julia
function VirtualPlantLab.feed!(turtle::Turtle, e::sn.E, data)
    HollowCylinder!(turtle, length = e.length, width = e.length/10,
                    height = e.length/10, move = true,
                    color = RGB(rand(), rand(), rand()))
    return nothing
end
```

The body of the method should generate the 3D structures using the geometry primitives provided by VPL and feed them to the turtle that is being passed to the method as first argument. In this case, we are going to represent the edges of the Koch snowflakes with cylinders, which can be generated with the `HollowCylinder!()` function. Note that `feed!` should return `nothing` as the turtle will be modified in place (it is not necessary to specify `return nothing` explicitly, but it helps for clarity).

In order to render the geometry, we need assign a `color` (i.e., any type of color support by the package ColorTypes.jl). In this case, we just feed a basic `RGB` color defined by the proportion of red, green and blue. To make the figures more appealing, we can assign random values to each channel of the color to generate random colors.

Note that the argument `move = true` indicates that the turtle should move forward by a distance equal to the length of the cylinder. Also, the `feed!` method has a third argument that may or may not be used (but should be in the function definition). This third argument will give access to the data stored at the graph level which may be useful in some scenarios.

The final step to generate geometry form a graph is to create a `Scene` object using the graph as input.

```julia
sc = Scene(Koch);
```

This will create a new turtle and use it to visit all the nodes in the graph starting from its roots. At each node it will call the method `feed!()` and store the generated geometry in an internal stack. To visualize the scene, we call the function `render()` with the scene as input (assuming we have imported a graphic backend such as GLMakie.jl):

```julia
render(sc, axes = false)
```

This renders the initial triangle of the construction procedure of the Koch snowflake. Let's execute the rules once to verify that we get the 2nd iteration (check the figure at the beginning of this document):

```julia
rewrite!(Koch)
render(Scene(Koch), axes = false)
```

And two more times

```julia
for i in 1:3
    rewrite!(Koch)
end
render(Scene(Koch), axes = false)
```

Play around with the zoom, panning and rotation functionality of the interactive visualization to get a better sense of the structure of the Koch snowflake. Do you recognize the recursive geometric patterns (fractals)? The shape of the snowflake is rather than complex, but we can see traces of the simple rule that gave birth to it in these patterns. By the way, you can always extract the total number of nodes in a graph with the function `length()`:

```julia
length(Koch)
```

### Other snowflake fractals

To demonstrate the power of this approach, let's create an alternative snowflake. We will simply invert the rotations of the turtle in the rewriting rule

```julia
function Kochsnowflake2(x)
   L = data(x).length
   sn.E(L/3) + RU(60.0) + sn.E(L/3) + RU(-120.0) + sn.E(L/3) + RU(60.0) + sn.E(L/3)
end
rule2 = Rule(sn.E, rhs = Kochsnowflake2)
Koch2 = Graph(axiom = axiom, rules = rule2)
```

The axiom is the same, but now the edges added by the rule will generate the edges towards the inside of the initial triangle. Let's execute the first three iterations and render the results

```julia
# First iteration
rewrite!(Koch2)
render(Scene(Koch2), axes = false)
# Second iteration
rewrite!(Koch2)
render(Scene(Koch2), axes = false)
# Third iteration
rewrite!(Koch2)
render(Scene(Koch2), axes = false)
```

This is know as [Koch antisnowflake](https://mathworld.wolfram.com/KochAntisnowflake.html).

We could also easily generate a [Cesàro fractal](https://mathworld.wolfram.com/CesaroFractal.html) by also changing the axiom:

```julia
axiomCesaro = sn.E(L) + RU(90.0) + sn.E(L) + RU(90.0) + sn.E(L) + RU(90.0) + sn.E(L)
Cesaro = Graph(axiom = axiomCesaro, rules = rule2)
render(Scene(Cesaro), axes = false)
```

And, as before, let's go through the first three iterations

```julia
# First iteration
rewrite!(Cesaro)
render(Scene(Cesaro), axes = false)
# Second iteration
rewrite!(Cesaro)
render(Scene(Cesaro), axes = false)
# Third iteration
rewrite!(Cesaro)
render(Scene(Cesaro), axes = false)
```

## Tutorial 03: Single tree

In this example we will be a simple, somewhat theoretical tree. This example will help introduce additional features of VPL and is followed by increasingly more detailed examples that add more functionality. Although we will not be modelling any particular species, the code in these tutorials may be used as a starting point for more realistic models.

The model requires five types of nodes to be used in the graphs:

*Meristem*: Apical meristems responsible for growth of new organs. They contain no data or geometry, but they will be used by rules that generate new internodes.

*Node*: What is left after a meristem produces a new organ. They contain no data or geometry but are required to keep the branching structure of the tree as well as connecting leaves to the branches they emerge from (i.e., lateral buds, leaves and internodes are connected to nodes).

*Internode*: A subset of a branch, defined between two (botanical) nodes. Internodes are represented by cylinders with a fixed width but variable length that will change over time.

*Bud*: These are dormant meristems associated to tree nodes (i.e., lateral buds). When they are activated, they become an active meristem that produces a branch. They contain no data or geometry but they are needed for the rules that create branches.

*BudNode*: The node left by a bud after it has been activated. They contain no data or geometry, but they change the orientation of the turtle.

*Leaf*: These are the nodes associated to leaves in the tree. They are represented by ellipses with a particular orientation and insertion angle. The insertion angle is assumed constant, but the orientation angle varies according to an elliptical phyllotaxis rule.

In this first simple model, only internodes grow over time according to a relative growth rate, whereas leaves are assumed to be of a fixed size determined at their creation. For simplicity, all active meristems will produce a phytomer (combination of node, internode, leaf and bud) per time step. Bud break is assumed stochastic, with a probability that increases proportional to the number of phytomers from the apical meristem (up to 1) to emulate apical dominance. In the following tutorials, these assumptions are replaced by more realistic models of light interception, photosynthesis, etc.

In order to simulate the growth of the tree, we need to define a parameter describing the relative rate at which each internode elongates in each iteration of the simulation, a coefficient to compute the probability of bud break as well as the insertion and orientation angles of the leaves. We could store these values as global constants, but VPL offers to opportunity to store them per plant. This makes it easier to manage multiple plants in the same simulation that may belong to different species, cultivars, ecotypes or simply to simulate plant-to-plant variation in phenotype. Graphs in VPL can store an object of any user-defined type that will be made accessible to graph rewriting rules and queries. For this example, we define a data type `treeparams` that holds the relevant parameters. We use `Base.@kwdef` to assign default values to all parameters and allow to assign them by name (this makes it easier to construct them).

```julia
using VirtualPlantLab

module TreeTypes
    import VirtualPlantLab
    # Meristem
    struct Meristem <: VirtualPlantLab.Node end
    # Bud
    struct Bud <: VirtualPlantLab.Node end
    # Node
    struct Node <: VirtualPlantLab.Node end
    # BudNode
    struct BudNode <: VirtualPlantLab.Node end
    # Internode (needs to be mutable to allow for changes over time)
    Base.@kwdef mutable struct Internode <: VirtualPlantLab.Node
        length::Float64 = 0.10 # Internodes start at 10 cm
    end
    # Leaf
    Base.@kwdef struct Leaf <: VirtualPlantLab.Node
        length::Float64 = 0.20 # Leaves are 20 cm long
        width::Float64  = 0.1 # Leaves are 10 cm wide
    end
    # Graph-level variables
    Base.@kwdef struct treeparams
        growth::Float64 = 0.1
        budbreak::Float64 = 0.25
        phyllotaxis::Float64 = 140.0
        leaf_angle::Float64 = 30.0
        branch_angle::Float64 = 45.0
    end
end
import .TreeTypes
```

As always, the 3D structure and the color of each type of node are implemented with the `feed!` method. In this case, the internodes and leaves have a 3D representation, whereas bud nodes rotate the turtle. The rest of the elements of the trees do not have a geometry associated to them:

```julia
# Create geometry + color for the internodes
function VirtualPlantLab.feed!(turtle::Turtle, i::TreeTypes.Internode, data)
    # Rotate turtle around the head to implement elliptical phyllotaxis
    rh!(turtle, data.phyllotaxis)
    HollowCylinder!(turtle, length = i.length, height = i.length/15, width = i.length/15,
                move = true, color = RGB(0.5,0.4,0.0))
    return nothing
end

# Create geometry + color for the leaves
function VirtualPlantLab.feed!(turtle::Turtle, l::TreeTypes.Leaf, data)
    # Rotate turtle around the arm for insertion angle
    ra!(turtle, -data.leaf_angle)
    # Generate the leaf
    Ellipse!(turtle, length = l.length, width = l.width, move = false,
             color = RGB(0.2,0.6,0.2))
    # Rotate turtle back to original direction
    ra!(turtle, data.leaf_angle)
    return nothing
end

# Insertion angle for the bud nodes
function VirtualPlantLab.feed!(turtle::Turtle, b::TreeTypes.BudNode, data)
    # Rotate turtle around the arm for insertion angle
    ra!(turtle, -data.branch_angle)
end
```

The growth rule for a branch within a tree is simple: a phytomer (or basic unit of morphology) is composed of a node, a leaf, a bud node, an internode and an active meristem at the end. Each time step, the meristem is replaced by a new phytomer, allowing for development within a branch.

```julia
phytomer() = TreeTypes.Node() + (TreeTypes.Bud(), TreeTypes.Leaf()) + TreeTypes.Internode()
meristem_rule = Rule(TreeTypes.Meristem, rhs = mer -> phytomer() + TreeTypes.Meristem())
```

For readability, we split the rule into different components rather than having one long rule. In general, it is best practice to create new nodes within a rule rather than copy pre-generated nodes to ensure that the nodes are independent. That is why we define a function `phytomer()` rather than a variable `phytomer` that would store a pre-generated phytomer.

In addition, in each step of the simulation, a bud may break, creating a new branch. The probability of bud break is proportional to the number of phytomers from the apical meristem (up to 1), which requires a relational rule to count the number of internodes in the graph up to reaching a meristem. When a bud breaks, it is replaced by a bud node, an internode and a new meristem. This new meristem becomes the apical meristem of the new branch, such that `meristem_rule` would apply in future iterations. Note how we create a dedicated function to compute whether a bud breaks or not. This is useful to keep the `branch_rule` rule simple and readable, while allowing for a relatively complex bud break model. It also makes it easier to debug the bud break model.

```julia
function prob_break(bud)
    # We move to parent node in the branch where the bud was created
    node =  parent(bud)
    # We count the number of internodes between node and the first meristem
    # moving down the graph
    check, steps = has_descendant(node, condition = n -> data(n) isa TreeTypes.Meristem)
    steps = Int(ceil(steps/2)) # Because it will count both the nodes and the internodes
    # Compute probability of bud break and determine whether it happens
    if check
        prob =  min(1.0, steps*graph_data(bud).budbreak)
        return rand() < prob
    # If there is no meristem, an error happened since the model does not allow for this
    else
        error("No meristem found in branch")
    end
end
branch_rule = Rule(TreeTypes.Bud,
            lhs = prob_break,
            rhs = bud -> TreeTypes.BudNode() + TreeTypes.Internode() + TreeTypes.Meristem())
```

A tree is initialized as an internode followed by a meristem, so the axiom can be constructed simply as:

```julia
axiom = TreeTypes.Internode() + TreeTypes.Meristem()
```

And the object for the tree can be constructed as in previous examples, by passing the axiom and the graph rewriting rules, but in this case also the object with growth-related parameters.

```julia
tree = Graph(axiom = axiom, rules = (meristem_rule, branch_rule), data = TreeTypes.treeparams())
```

Note that so far we have not included any code to simulate growth of the internodes. The reason is that, as the growth of internodes does not change the topology of the graph (it simply changes the data stored in certain nodes), this process does not need to be implemented with graph rewriting rules. Instead, we will use a combination of a query (to identify which nodes need to be altered) and direct modification of these nodes:

```julia
getInternode = Query(TreeTypes.Internode)
```

If we apply the query to a graph using the `apply` function, we will get an array of all the nodes that match the query, allow for direct manipulation of their contents. To help organize the code, we will create a function that simulates growth by multiplying the `length` argument of all internodes in a tree by the `growth` parameter defined in the above:

```julia
function elongate!(tree, query)
    for x in apply(tree, query)
        x.length = x.length*(1.0 + data(tree).growth)
    end
end
```

Note that we use `data()` on the `Graph` object to extract the data that was stored inside it. Also, as this function will modify the graph which is passed as input, we append a `!` to the name (this not a special syntax of the language, it is just a convention in the Julia community). Furthermore, in this case, the query object is kept separate from the graph. We could have also stored it inside the graph like we did for the parameter `growth`. We could also have packaged the graph and the query into another type representing an individual tree. This is entirely up to the user and indicates that a model can be implemented in many ways with VPL.

Simulating the growth a tree is a matter of elongating the internodes and applying the rules to create new internodes:

```julia
function growth!(tree, query)
    elongate!(tree, query)
    rewrite!(tree)
end
```

and a simulation for the n steps is achieved with a simple loop:

```julia
function simulate(tree, query, nsteps)
    new_tree = deepcopy(tree)
    for i in 1:nsteps
        growth!(new_tree, query)
    end
    return new_tree
end
```

Notice that the `simulate` function creates a deep copy of the object to avoid overwriting the original. If we run the simulation for a couple of steps

```julia
newtree = simulate(tree, getInternode, 2)
```

The resulting tree is rather boring right now:

```julia
render(Scene(newtree))
```

Further steps will generate a structure that is more tree-like.

```julia
newtree = simulate(newtree, getInternode, 15)
render(Scene(newtree))
```

Note that your results may differ, since the model is stochastic, and we did not fix the random seed of the random number generator.

## Tutorial 04: Forest

In this example we extend the tree example into a forest, where
each tree is described by a separate graph object and parameters driving the
growth of these trees vary across individuals following a predefined distribution.
The data types, rendering methods and growth rules are the same as in the tree
example:

```julia
using VirtualPlantLab
using Distributions, Plots
# Data types
module TreeTypes
    import VirtualPlantLab
    # Meristem
    struct Meristem <: VirtualPlantLab.Node end
    # Bud
    struct Bud <: VirtualPlantLab.Node end
    # Node
    struct Node <: VirtualPlantLab.Node end
    # BudNode
    struct BudNode <: VirtualPlantLab.Node end
    # Internode (needs to be mutable to allow for changes over time)
    Base.@kwdef mutable struct Internode <: VirtualPlantLab.Node
        length::Float64 = 0.10 # Internodes start at 10 cm
    end
    # Leaf
    Base.@kwdef struct Leaf <: VirtualPlantLab.Node
        length::Float64 = 0.20 # Leaves are 20 cm long
        width::Float64  = 0.1 # Leaves are 10 cm wide
    end
    # Graph-level variables
    Base.@kwdef struct treeparams
        growth::Float64 = 0.1
        budbreak::Float64 = 0.25
        phyllotaxis::Float64 = 140.0
        leaf_angle::Float64 = 30.0
        branch_angle::Float64 = 45.0
    end
end

import .TreeTypes

# Create geometry + color for the internodes
function VirtualPlantLab.feed!(turtle::Turtle, i::TreeTypes.Internode, data)
    # Rotate turtle around the head to implement elliptical phyllotaxis
    rh!(turtle, data.phyllotaxis)
    HollowCylinder!(turtle, length = i.length, height = i.length/15, width = i.length/15,
                move = true, color = RGB(0.5,0.4,0.0))
    return nothing
end

# Create geometry + color for the leaves
function VirtualPlantLab.feed!(turtle::Turtle, l::TreeTypes.Leaf, data)
    # Rotate turtle around the arm for insertion angle
    ra!(turtle, -data.leaf_angle)
    # Generate the leaf
    Ellipse!(turtle, length = l.length, width = l.width, move = false,
             color = RGB(0.2,0.6,0.2))
    # Rotate turtle back to original direction
    ra!(turtle, data.leaf_angle)
    return nothing
end

# Insertion angle for the bud nodes
function VirtualPlantLab.feed!(turtle::Turtle, b::TreeTypes.BudNode, data)
    # Rotate turtle around the arm for insertion angle
    ra!(turtle, -data.branch_angle)
end


# Rules
meristem_rule = Rule(TreeTypes.Meristem, rhs = mer -> TreeTypes.Node() +
                                              (TreeTypes.Bud(), TreeTypes.Leaf()) +
                                         TreeTypes.Internode() + TreeTypes.Meristem())

function prob_break(bud)
    # We move to parent node in the branch where the bud was created
    node =  parent(bud)
    # We count the number of internodes between node and the first Meristem
    # moving down the graph
    check, steps = has_descendant(node, condition = n -> data(n) isa TreeTypes.Meristem)
    steps = Int(ceil(steps/2)) # Because it will count both the nodes and the internodes
    # Compute probability of bud break and determine whether it happens
    if check
        prob =  min(1.0, steps*graph_data(bud).budbreak)
        return rand() < prob
    # If there is no meristem, an error happened since the model does not allow for this
    else
        error("No meristem found in branch")
    end
end
branch_rule = Rule(TreeTypes.Bud,
            lhs = prob_break,
            rhs = bud -> TreeTypes.BudNode() + TreeTypes.Internode() + TreeTypes.Meristem())
```

The main difference with respect to the tree is that several of the parameters
will vary per tree. Also, the location of the tree and initial orientation of
the turtle will also vary. To achieve this we need to:

(i) Add two additional initial nodes that move the turtle to the starting position
of each tree and rotates it.

(ii) Wrap the axiom, rules and the creation of the graph into a function that
takes the required parameters as inputs.

```julia
function create_tree(origin, growth, budbreak, orientation)
    axiom = T(origin) + RH(orientation) + TreeTypes.Internode() + TreeTypes.Meristem()
    tree =  Graph(axiom = axiom, rules = (meristem_rule, branch_rule),
                  data = TreeTypes.treeparams(growth = growth, budbreak = budbreak))
    return tree
end
```

The code for elongating the internodes to simulate growth remains the same as for
the binary tree example

```julia
getInternode = Query(TreeTypes.Internode)

function elongate!(tree, query)
    for x in apply(tree, query)
        x.length = x.length*(1.0 + data(tree).growth)
    end
end

function growth!(tree, query)
    elongate!(tree, query)
    rewrite!(tree)
end

function simulate(tree, query, nsteps)
    new_tree = deepcopy(tree)
    for i in 1:nsteps
        growth!(new_tree, query)
    end
    return new_tree
end
```

Let's simulate a forest of 10 x 10 trees with a distance between (and within) rows
of 2 meters. First we generate the original positions of the trees. For the
position we just need to pass a `Vec` object with the x, y, and z coordinates of
the location of each tree. The code below will generate a matrix with the coordinates:

```julia
origins = [Vec(i,j,0) for i = 1:2.0:20.0, j = 1:2.0:20.0]
```

We may assume that the initial orientation is uniformly distributed between 0 and 360 degrees:

```julia
orientations = [rand()*360.0 for i = 1:2.0:20.0, j = 1:2.0:20.0]
```

For the `growth` and `budbreak` parameters, we will assume that they follow a
LogNormal and Beta distribution, respectively. We can generate random
values from these distributions using the `Distributions` package. For the
relative growth rate:

```julia
growths = rand(LogNormal(-2, 0.3), 10, 10)
histogram(vec(growths))
```

And for the `budbreak` parameter:

```julia
budbreaks = rand(Beta(2.0, 10), 10, 10)
histogram(vec(budbreaks))
```

Now we can create our forest by calling the `create_tree` function we defined earlier
with the correct inputs per tree:

```julia
forest = vec(create_tree.(origins, growths, budbreaks, orientations));
```

By vectorizing `create_tree()` over the different arrays, we end up with an array
of trees. Each tree is a different Graph, with its own nodes, rewriting rules
and variables. This avoids having to create a large graph to include all the
plants in a simulation. Below we will run a simulation, first using a sequential
approach (i.e. using one core) and then using multiple cores in our computers (please check
https://docs.julialang.org/en/v1/manual/multi-threading/ if the different cores are not being used
as you may need to change some settings in your computer).

### Sequential simulation

We can simulate the growth of each tree by applying the method `simulate` to each
tree, creating a new version of the forest (the code below is an array comprehension)

```julia
newforest = [simulate(tree, getInternode, 2) for tree in forest];
```

And we can render the forest with the function `render` as in the tree example but passing
the whole forest at once

```julia
render(Scene(newforest))
```

If we iterate 4 more iterations we will start seeing the different individuals
diverging in size due to the differences in growth rates

```julia
newforest = [simulate(tree, getInternode, 15) for tree in newforest];
render(Scene(newforest))
```

### Multithreaded simulation

In the previous section, the simulation of growth was done sequentially, one tree
after another (since the growth of a tree only depends on its own parameters). However,
this can also be executed in multiple threads. In this case we use an explicit loop
and execute the iterations of the loop in multiple threads using the macro `@threads`.
Note that the rendering function can also be run in parallel (i.e., the geometry will be
generated separately for each plant and then merged together):

```julia
using Base.Threads
newforest = deepcopy(forest)
@threads for i in eachindex(forest)
    newforest[i] = simulate(forest[i], getInternode, 6)
end
render(Scene(newforest), parallel = true)
```

An alternative way to perform the simulation is to have an outer loop for each time step and an internal loop over the different trees. Although this approach is not required for this simple model, most FSP models will probably need such a scheme as growth of each individual plant will depend on competition for resources with neighboring plants. In this case, this approach would look as follows:

```julia
newforest = deepcopy(forest)
for step in 1:15
    @threads for i in eachindex(newforest)
        newforest[i] = simulate(newforest[i], getInternode, 1)
    end
end
render(Scene(newforest), parallel = true)
```

### Customizing the scene

Here we are going to customize the scene of our simulation by adding a horizontal tile representing soil and
tweaking the 3D representation. First we create the scene as usual:

```julia
scene = Scene(newforest);
```

We can create the soil tile directly, without having to create a graph. The simplest approach is to use
a special constructor `Rectangle` where one specifies a corner of the rectangle and two vectors defining the
two sides of the vectors. Both the sides and the corner need to be specified with `Vec` just like in the
above when we determined the origin of each plant. VPL offers some shortcuts: `O()` returns the origin
(`Vec(0.0, 0.0, 0.0)`), whereas `X`, `Y` and `Z` returns the corresponding axes, and you can scale them by
passing the desired length as input. Below, a rectangle is created on the XY plane with the origin as a
corner and each side being 11 units long:

```julia
soil = Rectangle(length = 21.0, width = 21.0)
rotatey!(soil, pi/2)
VirtualPlantLab.translate!(soil, Vec(0.0, 10.5, 0.0))
```

We can now add the `soil` to the `scene` object with the `add!` function.

```julia
VirtualPlantLab.add!(scene, mesh = soil, color = RGB(1,1,0))
```

We can now render the scene that combines the random forest of trees and a yellow soil. Notice that
in all previous figures, a coordinate system with grids was being depicted. This is helpful for debugging
your code but also to help set up the scene (e.g. if you are not sure how big the soil tile should be).
However, it may be distracting for the visualization. It turns out that we can turn that off with
`axes = false`:

```julia
render(scene, axes = false)
```

We may also want to save a screenshot of the scene. For this, we need to store the output of the `render` function.
We can then resize the window rendering the scene, move around, zoom, etc. When we have a perspective that we like,
we can run the `export_scene` function on the object returned from `render`. The argument `resolution` can be adjusted in
`render` to increase the number of pixels in the final image. A helper function `calculate_resolution` is provided to
compute the resolution from a physical width and height in cm and a dpi (e.g., useful for publications and posters):

```julia
res = calculate_resolution(width = 16.0, height = 16.0, dpi = 1_000)
output = render(scene, axes = false, resolution = res)
export_scene(scene = output, filename = "nice_trees.png")
```

## Tutorial 05: Growth-driven forest

In this example we extend the forest example to have more complex, time-
dependent development and growth based on carbon allocation. For simplicity, the
model assumes a constant relative growth rate at the plant level to compute the
biomass increment. In the next example this assumption is relaxed by a model of
radiation use efficiency. When modelling growth from carbon allocation, the
biomass of each organ is then translated in to an area or volume and the
dimensions of the organs are updated accordingly (assuming a particular shape).

The following packages are needed:

```julia
using VirtualPlantLab
using Base.Threads: @threads
using Plots
import Random
using Distributions
Random.seed!(123456789) # For reproducibility
```

### Model definition

#### Node types

The data types needed to simulate the trees are given in the following
module. The differences with respect to the previous example are:

  - Meristems do not produce phytomers every day
  - A relative sink strength approach is used to allocate biomass to organs
  - The geometry of the organs is updated based on the new biomass
  - Bud break probability is a function of distance to apical meristem rather than number of internodes

```julia
# Data types
module TreeTypes
    import VirtualPlantLab
    using Distributions
    # Meristem
    Base.@kwdef mutable struct Meristem <: VirtualPlantLab.Node
        age::Int64 = 0   # Age of the meristem
    end
    # Bud
    struct Bud <: VirtualPlantLab.Node end
    # Node
    struct Node <: VirtualPlantLab.Node end
    # BudNode
    struct BudNode <: VirtualPlantLab.Node end
    # Internode (needs to be mutable to allow for changes over time)
    Base.@kwdef mutable struct Internode <: VirtualPlantLab.Node
        age::Int64 = 0         # Age of the internode
        biomass::Float64 = 0.0 # Initial biomass
        length::Float64 = 0.0  # Internodes
        width::Float64  = 0.0  # Internodes
        sink::Exponential{Float64} = Exponential(5)
    end
    # Leaf
    Base.@kwdef mutable struct Leaf <: VirtualPlantLab.Node
        age::Int64 = 0         # Age of the leaf
        biomass::Float64 = 0.0 # Initial biomass
        length::Float64 = 0.0  # Leaves
        width::Float64 = 0.0   # Leaves
        sink::Beta{Float64} = Beta(2,5)
    end
    # Graph-level variables -> mutable because we need to modify them during growth
    Base.@kwdef mutable struct treeparams
        # Variables
        biomass::Float64 = 2e-3 # Current total biomass (g)
        # Parameters
        RGR::Float64 = 1.0   # Relative growth rate (1/d)
        IB0::Float64 = 1e-3  # Initial biomass of an internode (g)
        SIW::Float64 = 0.1e6   # Specific internode weight (g/m3)
        IS::Float64  = 15.0  # Internode shape parameter (length/width)
        LB0::Float64 = 1e-3  # Initial biomass of a leaf
        SLW::Float64 = 100.0 # Specific leaf weight (g/m2)
        LS::Float64  = 3.0   # Leaf shape parameter (length/width)
        budbreak::Float64 = 1/0.5 # Bud break probability coefficient (in 1/m)
        plastochron::Int64 = 5 # Number of days between phytomer production
        leaf_expansion::Float64 = 15.0 # Number of days that a leaf expands
        phyllotaxis::Float64 = 140.0
        leaf_angle::Float64 = 30.0
        branch_angle::Float64 = 45.0
    end
end

import .TreeTypes
```

#### Geometry

The methods for creating the geometry and color of the tree are the same as in
the previous example.

```julia
# Create geometry + color for the internodes
function VirtualPlantLab.feed!(turtle::Turtle, i::TreeTypes.Internode, vars)
    # Rotate turtle around the head to implement elliptical phyllotaxis
    rh!(turtle, vars.phyllotaxis)
    HollowCylinder!(turtle, length = i.length, height = i.width, width = i.width,
                move = true, color = RGB(0.5,0.4,0.0))
    return nothing
end

# Create geometry + color for the leaves
function VirtualPlantLab.feed!(turtle::Turtle, l::TreeTypes.Leaf, vars)
    # Rotate turtle around the arm for insertion angle
    ra!(turtle, -vars.leaf_angle)
    # Generate the leaf
    Ellipse!(turtle, length = l.length, width = l.width, move = false,
             color = RGB(0.2,0.6,0.2))
    # Rotate turtle back to original direction
    ra!(turtle, vars.leaf_angle)
    return nothing
end

# Insertion angle for the bud nodes
function VirtualPlantLab.feed!(turtle::Turtle, b::TreeTypes.BudNode, vars)
    # Rotate turtle around the arm for insertion angle
    ra!(turtle, -vars.branch_angle)
end
```

#### Development

The meristem rule is now parameterized by the initial states of the leaves and
internodes and will only be triggered every X days where X is the plastochron.

```julia
# Create right side of the growth rule (parameterized by the initial states
# of the leaves and internodes)
function create_meristem_rule(vleaf, vint)
    meristem_rule = Rule(TreeTypes.Meristem,
                        lhs = mer -> mod(data(mer).age, graph_data(mer).plastochron) == 0,
                        rhs = mer -> TreeTypes.Node() +
                                     (TreeTypes.Bud(),
                                     TreeTypes.Leaf(biomass = vleaf.biomass,
                                                    length  = vleaf.length,
                                                    width   = vleaf.width)) +
                                     TreeTypes.Internode(biomass = vint.biomass,
                                                         length  = vint.length,
                                                         width   = vint.width) +
                                     TreeTypes.Meristem())
end
```

The bud break probability is now a function of distance to the apical meristem
rather than the number of internodes. An ad hoc traversal is used to compute the
length of the main branch a bud belongs to (ignoring the lateral branches).

```julia
# Compute the probability that a bud breaks as function of distance to the meristem
function prob_break(bud)
    # We move to parent node in the branch where the bud was created
    node =  parent(bud)
    # Extract the first internode
    child = filter(x -> data(x) isa TreeTypes.Internode, children(node))[1]
    data_child = data(child)
    # We measure the length of the branch until we find the meristem
    distance = 0.0
    while !isa(data_child, TreeTypes.Meristem)
        # If we encounter an internode, store the length and move to the next node
        if data_child isa TreeTypes.Internode
            distance += data_child.length
            child = children(child)[1]
            data_child = data(child)
        # If we encounter a node, extract the next internode
        elseif data_child isa TreeTypes.Node
                child = filter(x -> data(x) isa TreeTypes.Internode, children(child))[1]
                data_child = data(child)
        else
            error("Should be Internode, Node or Meristem")
        end
    end
    # Compute the probability of bud break as function of distance and
    # make stochastic decision
    prob =  min(1.0, distance*graph_data(bud).budbreak)
    return rand() < prob
end

# Branch rule parameterized by initial states of internodes
function create_branch_rule(vint)
    branch_rule = Rule(TreeTypes.Bud,
            lhs = prob_break,
            rhs = bud -> TreeTypes.BudNode() +
                         TreeTypes.Internode(biomass = vint.biomass,
                                             length  = vint.length,
                                             width   = vint.width) +
                         TreeTypes.Meristem())
end
```


#### Growth

We need some functions to compute the length and width of a leaf or internode
from its biomass

```julia
function leaf_dims(biomass, vars)
    leaf_biomass = biomass
    leaf_area    = biomass/vars.SLW
    leaf_length  = sqrt(leaf_area*4*vars.LS/pi)
    leaf_width   = leaf_length/vars.LS
    return leaf_length, leaf_width
end

function int_dims(biomass, vars)
    int_biomass = biomass
    int_volume  = biomass/vars.SIW
    int_length  = cbrt(int_volume*4*vars.IS^2/pi)
    int_width   = int_length/vars.IS
    return int_length, int_width
end
```

Each day, the total biomass of the tree is updated using a simple RGR formula
and the increment of biomass is distributed across the organs proportionally to
their relative sink strength (of leaves or internodes).

The sink strength of leaves is modelled with a beta distribution scaled to the
`leaf_expansion` argument that determines the duration of leaf growth, whereas
for the internodes it follows a negative exponential distribution. The `pdf`
function computes the probability density of each distribution which is taken as
proportional to the sink strength (the model is actually source-limited since we
imposed a particular growth rate).

```julia
sink_strength(leaf, vars) = leaf.age > vars.leaf_expansion ? 0.0 :
                            pdf(leaf.sink, leaf.age/vars.leaf_expansion)/100.0
plot(0:1:50, x -> sink_strength(TreeTypes.Leaf(age = x), TreeTypes.treeparams()),
     xlabel = "Age", ylabel = "Sink strength", label = "Leaf")
```

```julia
sink_strength(int) = pdf(int.sink, int.age)
plot!(0:1:50, x -> sink_strength(TreeTypes.Internode(age = x)), label = "Internode")
```

Now we need a function that updates the biomass of a tree, allocates it to the
different organs and updates the dimensions of said organs. For simplicity,
we create the functions `leaves()` and `internodes()` that will apply the queries
to the tree required to extract said nodes:

```julia
get_leaves(tree) = apply(tree, Query(TreeTypes.Leaf))
get_internodes(tree) = apply(tree, Query(TreeTypes.Internode))
```

The age of the different organs is updated every time step:

```julia
function age!(all_leaves, all_internodes, all_meristems)
    for leaf in all_leaves
        leaf.age += 1
    end
    for int in all_internodes
        int.age += 1
    end
    for mer in all_meristems
        mer.age += 1
    end
    return nothing
end
```

The daily growth is allocated to different organs proportional to their sink strength.

```julia
function grow!(tree, all_leaves, all_internodes)
    # Compute total biomass increment
    tvars = data(tree)
    ΔB    = tvars.RGR*tvars.biomass
    tvars.biomass += ΔB
    # Total sink strength
    total_sink = 0.0
    for leaf in all_leaves
        total_sink += sink_strength(leaf, tvars)
    end
    for int in all_internodes
        total_sink += sink_strength(int)
    end
    # Allocate biomass to leaves and internodes
    for leaf in all_leaves
        leaf.biomass += ΔB*sink_strength(leaf, tvars)/total_sink
    end
    for int in all_internodes
        int.biomass += ΔB*sink_strength(int)/total_sink
    end
    return nothing
end
```

Finally, we need to update the dimensions of the organs. The leaf dimensions are

```julia
function size_leaves!(all_leaves, tvars)
    for leaf in all_leaves
        leaf.length, leaf.width = leaf_dims(leaf.biomass, tvars)
    end
    return nothing
end
function size_internodes!(all_internodes, tvars)
    for int in all_internodes
        int.length, int.width = int_dims(int.biomass, tvars)
    end
    return nothing
end
```

#### Daily step

All the growth and developmental functions are combined into a daily step function that
updates the forest by iterating over the different trees in parallel.

```julia
get_meristems(tree) = apply(tree, Query(TreeTypes.Meristem))
function daily_step!(forest)
    @threads for tree in forest
        # Retrieve all the relevant organs
        all_leaves = get_leaves(tree)
        all_internodes = get_internodes(tree)
        all_meristems = get_meristems(tree)
        # Update the age of the organs
        age!(all_leaves, all_internodes, all_meristems)
        # Grow the tree
        grow!(tree, all_leaves, all_internodes)
        tvars = data(tree)
        size_leaves!(all_leaves, tvars)
        size_internodes!(all_internodes, tvars)
        # Developmental rules
        rewrite!(tree)
    end
end
```

#### Initialization

The trees are initialized in a regular grid with random values for the initial
orientation and RGR:

```julia
RGRs = rand(Normal(0.3,0.01), 10, 10)
histogram(vec(RGRs))
```

```julia
orientations = [rand()*360.0 for i = 1:2.0:20.0, j = 1:2.0:20.0]
histogram(vec(orientations))
```

```julia
origins = [Vec(i,j,0) for i = 1:2.0:20.0, j = 1:2.0:20.0];
```

The following initializes a tree based on the origin, orientation and RGR:

```julia
function create_tree(origin, orientation, RGR)
    # Initial state and parameters of the tree
    vars = TreeTypes.treeparams(RGR = RGR)
    # Initial states of the leaves
    leaf_length, leaf_width = leaf_dims(vars.LB0, vars)
    vleaf = (biomass = vars.LB0, length = leaf_length, width = leaf_width)
    # Initial states of the internodes
    int_length, int_width = int_dims(vars.LB0, vars)
    vint = (biomass = vars.IB0, length = int_length, width = int_width)
    # Growth rules
    meristem_rule = create_meristem_rule(vleaf, vint)
    branch_rule   = create_branch_rule(vint)
    axiom = T(origin) + RH(orientation) +
            TreeTypes.Internode(biomass = vint.biomass,
                             length  = vint.length,
                             width   = vint.width) +
            TreeTypes.Meristem()
    tree = Graph(axiom = axiom, rules = (meristem_rule, branch_rule),
                 data = vars)
    return tree
end
```

### Visualization

As in the previous example, it makes sense to visualize the forest with a soil
tile beneath it. Unlike in the previous example, we will construct the soil tile
using a dedicated graph and generate a `Scene` object which can later be
merged with the rest of scene generated in daily step:

```julia
Base.@kwdef struct Soil <: VirtualPlantLab.Node
    length::Float64
    width::Float64
end
function VirtualPlantLab.feed!(turtle::Turtle, s::Soil, vars)
    Rectangle!(turtle, length = s.length, width = s.width, color = RGB(255/255, 236/255, 179/255))
end
soil_graph = RA(-90.0) + T(Vec(0.0, 10.0, 0.0)) + # Moves into position
             Soil(length = 20.0, width = 20.0) # Draws the soil tile
soil = Scene(Graph(axiom = soil_graph));
render(soil, axes = false)
```

And the following function creates and renders the scene:

```julia
function render_forest(forest, soil)
    scene = Scene(vec(forest)) # create scene from forest
    scene = Scene([scene, soil]) # merges the two scenes
    render(scene)
end
```

### Retrieving canopy-level data

We may want to extract some information at the canopy level such as LAI. This is
best achieved with a query:

```julia
function get_LAI(forest)
    LAI = 0.0
    @threads for tree in forest
        for leaf in get_leaves(tree)
            LAI += leaf.length*leaf.width*pi/4.0
        end
    end
    return LAI/400.0
end
```

### Simulation

We can now create a forest of trees on a regular grid:

```julia
forest = create_tree.(origins, orientations, RGRs);
render_forest(forest, soil)
for i in 1:50
    daily_step!(forest)
end
render_forest(forest, soil)
```

And compute the leaf area index:

```julia
get_LAI(forest)
```

## Tutorial 06: RUE-driven forest

In this example we extend the forest growth model to include PAR interception a
radiation use efficiency to compute the daily growth rate.

The following packages are needed:

```julia
using VirtualPlantLab
using SkyDomes
using Plots
using Distributions
import Random
Random.seed!(123456789)
using Base.Threads: @threads
```

### Model definition

#### Node types

The data types needed to simulate the trees are given in the following module. The
difference with respect to the previous model is that `Internodes` and `Leaves` have optical
properties needed for ray tracing (they are defined as Lambertian surfaces).

```julia
# Data types
module TreeTypes
    import VirtualPlantLab as VPL
    using Distributions
    # Meristem
    Base.@kwdef mutable struct Meristem <: VPL.Node
        age::Int64 = 0   # Age of the meristem
    end
    # Bud
    struct Bud <: VPL.Node end
    # Node
    struct Node <: VPL.Node end
    # BudNode
    struct BudNode <: VPL.Node end
    # Internode (needs to be mutable to allow for changes over time)
    Base.@kwdef mutable struct Internode <: VPL.Node
        age::Int64 = 0         # Age of the internode
        biomass::Float64 = 0.0 # Initial biomass
        length::Float64 = 0.0  # Internodes
        width::Float64  = 0.0  # Internodes
        sink::Exponential{Float64} = Exponential(5)
        material::VPL.Lambertian{1} = VPL.Lambertian(τ = 0.1, ρ = 0.05) # Leaf material
    end
    # Leaf
    Base.@kwdef mutable struct Leaf <: VPL.Node
        age::Int64 = 0         # Age of the leaf
        biomass::Float64 = 0.0 # Initial biomass
        length::Float64 = 0.0  # Leaves
        width::Float64 = 0.0   # Leaves
        sink::Beta{Float64} = Beta(2,5)
        material::VPL.Lambertian{1} = VPL.Lambertian(τ = 0.1, ρ = 0.05) # Leaf material
    end
    # Graph-level variables -> mutable because we need to modify them during growth
    Base.@kwdef mutable struct treeparams
        # Variables
        PAR::Float64 = 0.0   # Total PAR absorbed by the leaves on the tree (MJ)
        biomass::Float64 = 2e-3 # Current total biomass (g)
        # Parameters
        RUE::Float64 = 5.0   # Radiation use efficiency (g/MJ) -> unrealistic to speed up sim
        IB0::Float64 = 1e-3  # Initial biomass of an internode (g)
        SIW::Float64 = 0.1e6   # Specific internode weight (g/m3)
        IS::Float64  = 15.0  # Internode shape parameter (length/width)
        LB0::Float64 = 1e-3  # Initial biomass of a leaf
        SLW::Float64 = 100.0 # Specific leaf weight (g/m2)
        LS::Float64  = 3.0   # Leaf shape parameter (length/width)
        budbreak::Float64 = 1/0.5 # Bud break probability coefficient (in 1/m)
        plastochron::Int64 = 5 # Number of days between phytomer production
        leaf_expansion::Float64 = 15.0 # Number of days that a leaf expands
        phyllotaxis::Float64 = 140.0
        leaf_angle::Float64 = 30.0
        branch_angle::Float64 = 45.0
    end
end

import .TreeTypes
```

#### Geometry

The methods for creating the geometry and color of the tree are the same as in
the previous example but include the materials for the ray tracer.

```julia
# Create geometry + color for the internodes
function VirtualPlantLab.feed!(turtle::Turtle, i::TreeTypes.Internode, data)
    # Rotate turtle around the head to implement elliptical phyllotaxis
    rh!(turtle, data.phyllotaxis)
    HollowCylinder!(turtle, length = i.length, height = i.width, width = i.width,
                move = true, color = RGB(0.5,0.4,0.0), material = i.material)
    return nothing
end

# Create geometry + color for the leaves
function VirtualPlantLab.feed!(turtle::Turtle, l::TreeTypes.Leaf, data)
    # Rotate turtle around the arm for insertion angle
    ra!(turtle, -data.leaf_angle)
    # Generate the leaf
    Ellipse!(turtle, length = l.length, width = l.width, move = false,
             color = RGB(0.2,0.6,0.2), material = l.material)
    # Rotate turtle back to original direction
    ra!(turtle, data.leaf_angle)
    return nothing
end

# Insertion angle for the bud nodes
function VirtualPlantLab.feed!(turtle::Turtle, b::TreeTypes.BudNode, data)
    # Rotate turtle around the arm for insertion angle
    ra!(turtle, -data.branch_angle)
end
```

#### Development

The meristem rule is now parameterized by the initial states of the leaves and
internodes and will only be triggered every X days, where X is the plastochron.

```julia
# Create right side of the growth rule (parameterized by the initial states
# of the leaves and internodes)
function create_meristem_rule(vleaf, vint)
    meristem_rule = Rule(TreeTypes.Meristem,
                        lhs = mer -> mod(data(mer).age, graph_data(mer).plastochron) == 0,
                        rhs = mer -> TreeTypes.Node() +
                                     (TreeTypes.Bud(),
                                     TreeTypes.Leaf(biomass = vleaf.biomass,
                                                    length  = vleaf.length,
                                                    width   = vleaf.width)) +
                                     TreeTypes.Internode(biomass = vint.biomass,
                                                         length  = vint.length,
                                                         width   = vint.width) +
                                     TreeTypes.Meristem())
end
```

The bud break probability is now a function of distance to the apical meristem
rather than the number of internodes. An ad hoc traversal is used to compute this
length of the main branch a bud belongs to (ignoring the lateral branches).

```julia
# Compute the probability that a bud breaks as function of distance to the meristem
function prob_break(bud)
    # We move to parent node in the branch where the bud was created
    node =  parent(bud)
    # Extract the first internode
    child = filter(x -> data(x) isa TreeTypes.Internode, children(node))[1]
    data_child = data(child)
    # We measure the length of the branch until we find the meristem
    distance = 0.0
    while !isa(data_child, TreeTypes.Meristem)
        # If we encounter an internode, store the length and move to the next node
        if data_child isa TreeTypes.Internode
            distance += data_child.length
            child = children(child)[1]
            data_child = data(child)
        # If we encounter a node, extract the next internode
        elseif data_child isa TreeTypes.Node
                child = filter(x -> data(x) isa TreeTypes.Internode, children(child))[1]
                data_child = data(child)
        else
            error("Should be Internode, Node or Meristem")
        end
    end
    # Compute the probability of bud break as function of distance and
    # make stochastic decision
    prob =  min(1.0, distance*graph_data(bud).budbreak)
    return rand() < prob
end

# Branch rule parameterized by initial states of internodes
function create_branch_rule(vint)
    branch_rule = Rule(TreeTypes.Bud,
            lhs = prob_break,
            rhs = bud -> TreeTypes.BudNode() +
                         TreeTypes.Internode(biomass = vint.biomass,
                                             length  = vint.length,
                                             width   = vint.width) +
                         TreeTypes.Meristem())
end
```

#### Light interception

As growth is now dependent on intercepted PAR via RUE, we now need to simulate
light interception by the trees. We will use a ray-tracing approach to do so.
The first step is to create a scene with the trees and the light sources. As for
rendering, the scene can be created from the `forest` object by simply calling
`Scene(forest)` that will generate the 3D meshes and connect them to their
optical properties.

However, we also want to add the soil surface as this will affect the light
distribution within the scene due to reflection from the soil surface. This is
similar to the customized scene that we created before for rendering, but now
for the light simulation.

```julia
function create_soil()
    soil = Rectangle(length = 21.0, width = 21.0)
    rotatey!(soil, π/2) # To put it in the XY plane
    VirtualPlantLab.translate!(soil, Vec(0.0, 10.5, 0.0)) # Corner at (0,0,0)
    return soil
end
function create_scene(forest)
    # These are the trees
    scene = Scene(vec(forest))
    # Add a soil surface
    soil = create_soil()
    soil_material = Lambertian(τ = 0.0, ρ = 0.21)
    add!(scene, mesh = soil, material = soil_material)
    # Return the scene
    return scene
end
```

Given the scene, we can create the light sources that approximate the solar
irradiance on a given day, location and time of the day using the functions from
the SkyDomes.jl package (see package documentation for details). Given the latitude,
day of year and fraction of the day (`f = 0` being sunrise and `f = 1` being sunset),
the function `clear_sky()` computes the direct and diffuse solar radiation assuming
a clear sky. These values may be converted to different wavebands and units using
`waveband_conversion()`. Finally, the collection of light sources approximating
the solar irradiance distribution over the sky hemisphere is constructed with the
function `sky()` (this last step requires the 3D scene as input in order to place
the light sources adequately).

```julia
function create_sky(;scene, lat = 52.0*π/180.0, DOY = 182)
    # Fraction of the day and day length
    fs = collect(0.1:0.1:0.9)
    dec = declination(DOY)
    DL = day_length(lat, dec)*3600
    # Compute solar irradiance
    temp = [clear_sky(lat = lat, DOY = DOY, f = f) for f in fs] # W/m2
    Ig   = getindex.(temp, 1)
    Idir = getindex.(temp, 2)
    Idif = getindex.(temp, 3)
    # Conversion factors to PAR for direct and diffuse irradiance
    f_dir = waveband_conversion(Itype = :direct,  waveband = :PAR, mode = :power)
    f_dif = waveband_conversion(Itype = :diffuse, waveband = :PAR, mode = :power)
    # Actual irradiance per waveband
    Idir_PAR = f_dir.*Idir
    Idif_PAR = f_dif.*Idif
    # Create the dome of diffuse light
    dome = sky(scene,
                  Idir = 0.0, # No direct solar radiation
                  Idif = sum(Idir_PAR)/10*DL, # Daily Diffuse solar radiation
                  nrays_dif = 1_000_000, # Total number of rays for diffuse solar radiation
                  sky_model = StandardSky, # Angular distribution of solar radiation
                  dome_method = equal_solid_angles, # Discretization of the sky dome
                  ntheta = 9, # Number of discretization steps in the zenith angle
                  nphi = 12) # Number of discretization steps in the azimuth angle
    # Add direct sources for different times of the day
    for I in Idir_PAR
        push!(dome, sky(scene, Idir = I/10*DL, nrays_dir = 100_000, Idif = 0.0)[1])
    end
    return dome
end
```

The 3D scene and the light sources are then combined into a `RayTracer` object,
together with general settings for the ray tracing simulation chosen via `RTSettings()`.
The most important settings refer to the Russian roulette system and the grid
cloner (see documentation for details). The settings for the Russian
roulette system include the number of times a ray will be traced
deterministically (`maxiter`) and the probability that a ray that exceeds `maxiter`
is terminated (`pkill`). The grid cloner is used to approximate an infinite canopy
by replicating the scene in the different directions (`nx` and `ny` being the
number of replicates in each direction along the x and y axes, respectively). It
is also possible to turn on parallelization of the ray tracing simulation by
setting `parallel = true` (currently this uses Julia's built-in multithreading
capabilities).

In addition to `RTSettings()`, an acceleration structure and a splitting rule can
be defined when creating the `RayTracer` object (see ray tracing documentation
for details). The acceleration structure allows speeding up the ray tracing
by avoiding testing all rays against all triangles in the scene.

```julia
function create_raytracer(scene, sources)
    settings = RTSettings(pkill = 0.9, maxiter = 4, nx = 5, ny = 5, dx = 20.0,
                          dy = 20.0, parallel = true)
    RayTracer(scene, sources, settings = settings, acceleration = BVH,
                     rule = SAH{6}(1,20));
end
```

The actual ray tracing simulation is performed by calling the `trace!()` method
on the ray tracing object. This will trace all rays from all light sources and
update the radiant power absorbed by the different surfaces in the scene inside
the `Material` objects (see `feed!()` above):

```julia
function run_raytracer!(forest; DOY = 182)
    scene   = create_scene(forest)
    sources = create_sky(scene = scene, DOY = DOY)
    rtobj   = create_raytracer(scene, sources)
    trace!(rtobj)
    return nothing
end
```

The total PAR absorbed for each tree is calculated from the material objects of
the different internodes (using `power()` on the `Material` object). Note that
the `power()` function returns three different values, one for each waveband,
but they are added together as RUE is defined for total PAR.


```julia
# Run the ray tracer, calculate PAR absorbed per tree and add it to the daily
# total using general weighted quadrature formula
function calculate_PAR!(forest;  DOY = 182)
    # Reset PAR absorbed by the tree (at the start of a new day)
    reset_PAR!(forest)
    # Run the ray tracer to compute daily PAR absorption
    run_raytracer!(forest, DOY = DOY)
    # Add up PAR absorbed by each leaf within each tree
    @threads for tree in forest
        for l in get_leaves(tree)
            data(tree).PAR += power(l.material)[1]
        end
    end
    return nothing
end

# Reset PAR absorbed by the tree (at the start of a new day)
function reset_PAR!(forest)
    for tree in forest
        data(tree).PAR = 0.0
    end
    return nothing
end
```

#### Growth

We need some functions to compute the length and width of a leaf or internode
from its biomass

```julia
function leaf_dims(biomass, vars)
    leaf_biomass = biomass
    leaf_area    = biomass/vars.SLW
    leaf_length  = sqrt(leaf_area*4*vars.LS/pi)
    leaf_width   = leaf_length/vars.LS
    return leaf_length, leaf_width
end

function int_dims(biomass, vars)
    int_biomass = biomass
    int_volume  = biomass/vars.SIW
    int_length  = cbrt(int_volume*4*vars.IS^2/pi)
    int_width   = int_length/vars.IS
    return int_length, int_width
end
```

Each day, the total biomass of the tree is updated using a simple RUE formula
and the increment of biomass is distributed across the organs proportionally to
their relative sink strength (of leaves or internodes).

The sink strength of leaves is modelled with a beta distribution scaled to the
`leaf_expansion` argument that determines the duration of leaf growth, whereas
for the internodes it follows a negative exponential distribution. The `pdf`
function computes the probability density of each distribution which is taken as
proportional to the sink strength (the model is actually source-limited since we
imposed a particular growth rate).

```julia
sink_strength(leaf, vars) = leaf.age > vars.leaf_expansion ? 0.0 :
                            pdf(leaf.sink, leaf.age/vars.leaf_expansion)/100.0
plot(0:1:50, x -> sink_strength(TreeTypes.Leaf(age = x), TreeTypes.treeparams()),
     xlabel = "Age", ylabel = "Sink strength", label = "Leaf")
```

```julia
sink_strength(int) = pdf(int.sink, int.age)
plot!(0:1:50, x -> sink_strength(TreeTypes.Internode(age = x)), label = "Internode")
```

Now we need a function that updates the biomass of the tree, allocates it to the
different organs and updates the dimensions of said organs. For simplicity,
we create the functions `get_leaves()` and `get_internodes()` that will apply the queries
to the tree required to extract said nodes:

```julia
get_leaves(tree) = apply(tree, Query(TreeTypes.Leaf))
get_internodes(tree) = apply(tree, Query(TreeTypes.Internode))
```

The age of the different organs is updated every time step:

```julia
function age!(all_leaves, all_internodes, all_meristems)
    for leaf in all_leaves
        leaf.age += 1
    end
    for int in all_internodes
        int.age += 1
    end
    for mer in all_meristems
        mer.age += 1
    end
    return nothing
end
```

The daily growth is allocated to different organs proportional to their sink strength.

```julia
function grow!(tree, all_leaves, all_internodes)
    # Compute total biomass increment
    tvars = data(tree)
    ΔB    = max(0.5, tvars.RUE*tvars.PAR/1e6) # Trick to emulate reserves in seedling
    tvars.biomass += ΔB
    # Total sink strength
    total_sink = 0.0
    for leaf in all_leaves
        total_sink += sink_strength(leaf, tvars)
    end
    for int in all_internodes
        total_sink += sink_strength(int)
    end
    # Allocate biomass to leaves and internodes
    for leaf in all_leaves
        leaf.biomass += ΔB*sink_strength(leaf, tvars)/total_sink
    end
    for int in all_internodes
        int.biomass += ΔB*sink_strength(int)/total_sink
    end
    return nothing
end
```

Finally, we need to update the dimensions of the organs. The leaf dimensions are:

```julia
function size_leaves!(all_leaves, tvars)
    for leaf in all_leaves
        leaf.length, leaf.width = leaf_dims(leaf.biomass, tvars)
    end
    return nothing
end
function size_internodes!(all_internodes, tvars)
    for int in all_internodes
        int.length, int.width = int_dims(int.biomass, tvars)
    end
    return nothing
end
```

#### Daily step

All the growth and developmental functions are combined into a daily step function that
updates the forest by iterating over the different trees in parallel.

```julia
get_meristems(tree) = apply(tree, Query(TreeTypes.Meristem))
function daily_step!(forest, DOY)
    # Compute PAR absorbed by each tree
    calculate_PAR!(forest, DOY = DOY)
    # Grow the trees
    @threads for tree in forest
        # Retrieve all the relevant organs
        all_leaves = get_leaves(tree)
        all_internodes = get_internodes(tree)
        all_meristems = get_meristems(tree)
        # Update the age of the organs
        age!(all_leaves, all_internodes, all_meristems)
        # Grow the tree
        grow!(tree, all_leaves, all_internodes)
        tvars = data(tree)
        size_leaves!(all_leaves, tvars)
        size_internodes!(all_internodes, tvars)
        # Developmental rules
        rewrite!(tree)
    end
end
```

#### Initialization

The trees are initialized on a regular grid with random values for the initial orientation
and RUE:

```julia
RUEs = rand(Normal(1.5,0.2), 10, 10)
histogram(vec(RUEs))
```

```julia
orientations = [rand()*360.0 for i = 1:2.0:20.0, j = 1:2.0:20.0]
histogram(vec(orientations))
```

```julia
origins = [Vec(i,j,0) for i = 1:2.0:20.0, j = 1:2.0:20.0];
```

The following initializes a tree based on the origin, orientation and RUE:

```julia
function create_tree(origin, orientation, RUE)
    # Initial state and parameters of the tree
    vars = TreeTypes.treeparams(RUE = RUE)
    # Initial states of the leaves
    leaf_length, leaf_width = leaf_dims(vars.LB0, vars)
    vleaf = (biomass = vars.LB0, length = leaf_length, width = leaf_width)
    # Initial states of the internodes
    int_length, int_width = int_dims(vars.LB0, vars)
    vint = (biomass = vars.IB0, length = int_length, width = int_width)
    # Growth rules
    meristem_rule = create_meristem_rule(vleaf, vint)
    branch_rule   = create_branch_rule(vint)
    axiom = T(origin) + RH(orientation) +
            TreeTypes.Internode(biomass = vint.biomass,
                                length  = vint.length,
                                width   = vint.width) +
            TreeTypes.Meristem()
    tree = Graph(axiom = axiom, rules = (meristem_rule, branch_rule),
                 data = vars)
    return tree
end
```


### Visualization

As in the previous example, it makes sense to visualize the forest with a soil
tile beneath it. Unlike in the previous example, we will construct the soil tile
using a dedicated graph and generate a `Scene` object which can later be
merged with the rest of scene generated in daily step:

```julia
Base.@kwdef struct Soil <: VirtualPlantLab.Node
    length::Float64
    width::Float64
end
function VirtualPlantLab.feed!(turtle::Turtle, s::Soil, vars)
    Rectangle!(turtle, length = s.length, width = s.width, color = RGB(255/255, 236/255, 179/255))
end
soil_graph = RA(-90.0) + T(Vec(0.0, 10.0, 0.0)) + # Moves into position
             Soil(length = 20.0, width = 20.0) # Draws the soil tile
soil = Scene(Graph(axiom = soil_graph));
import GLMakie
render(soil, axes = false)
```

And the following function renders the entire scene:

```julia
function render_forest(forest, soil)
    scene = Scene(vec(forest)) # create scene from forest
    scene = Scene([scene, soil]) # merges the two scenes
    display(render(scene))
end
```

### Simulation

We can now create a forest of trees on a regular grid:

```julia
forest = create_tree.(origins, orientations, RUEs);
render_forest(forest, soil)
start = 180
for i in 1:5
    println("Day $i")
    daily_step!(forest, i + start)
    if mod(i, 5) == 0
        render_forest(forest, soil)
    end
end
```


## Tutorial 07: Canopy photosynthesis

In this tutorial we will add photosynthesis calculations to the forest model (for
simplicity we will still grow the trees descriptively, but this could be extended
to a full growth model including respiration, carbon allocation, etc.).

We start with the code from Tutorial 04 with the following additions:

  * Load the Ecophys.jl package
  * Add materials to internodes, leaves and soil tile
  * Keep track of absorbed PAR within each leaf
  * Compute daily photosynthesis for each leaf using Gaussian-Legendre integration over the day
  * Integrate to the tree level

```julia
using VirtualPlantLab, Distributions, Plots, Ecophys, SkyDomes, FastGaussQuadrature
import Base.Threads: @threads
import Random
Random.seed!(123456789)
import GLMakie
# Data types
module TreeTypes
    import VirtualPlantLab as VPL
    import Ecophys
    # Meristem
    struct Meristem <: VPL.Node end
    # Bud
    struct Bud <: VPL.Node end
    # Node
    struct Node <: VPL.Node end
    # BudNode
    struct BudNode <: VPL.Node end
    # Internode (needs to be mutable to allow for changes over time)
    Base.@kwdef mutable struct Internode <: VPL.Node
        length::Float64 = 0.10 # Internodes start at 10 cm
        mat::VPL.Lambertian{1} = VPL.Lambertian(τ = 0.00, ρ = 0.05)
    end
    # Leaf
    Base.@kwdef mutable struct Leaf <: VPL.Node
        length::Float64 = 0.20 # Leaves are 20 cm long
        width::Float64  = 0.1 # Leaves are 10 cm wide
        PARdif::Float64 = 0.0
        PARdir::Float64 = 0.0
        mat::VPL.Lambertian{1} = VPL.Lambertian(τ = 0.05, ρ = 0.1)
        Ag::Float64 = 0.0
    end
    # Graph-level variables
    Base.@kwdef mutable struct treeparams
        growth::Float64 = 0.1
        budbreak::Float64 = 0.25
        phyllotaxis::Float64 = 140.0
        leaf_angle::Float64 = 30.0
        branch_angle::Float64 = 45.0
        photos::Ecophys.C3{Float64} = Ecophys.C3()
        Ag::Float64 = 0.0
    end
end

import .TreeTypes

# Create geometry + color for the internodes
function VirtualPlantLab.feed!(turtle::Turtle, i::TreeTypes.Internode, vars)
    # Rotate turtle around the head to implement elliptical phyllotaxis
    rh!(turtle, vars.phyllotaxis)
    HollowCylinder!(turtle, length = i.length, height = i.length/15, width = i.length/15,
                move = true, color = RGB(0.5,0.4,0.0), material = i.mat)
    return nothing
end

# Create geometry + color for the leaves
function VirtualPlantLab.feed!(turtle::Turtle, l::TreeTypes.Leaf, vars)
    # Rotate turtle around the arm for insertion angle
    ra!(turtle, -vars.leaf_angle)
    # Generate the leaf
    Ellipse!(turtle, length = l.length, width = l.width, move = false,
             color = RGB(0.2,0.6,0.2), material = l.mat)
    # Rotate turtle back to original direction
    ra!(turtle, vars.leaf_angle)
    return nothing
end

# Insertion angle for the bud nodes
function VirtualPlantLab.feed!(turtle::Turtle, b::TreeTypes.BudNode, vars)
    # Rotate turtle around the arm for insertion angle
    ra!(turtle, -vars.branch_angle)
end


# Rules
meristem_rule = Rule(TreeTypes.Meristem, rhs = mer -> TreeTypes.Node() +
                                              (TreeTypes.Bud(), TreeTypes.Leaf()) +
                                         TreeTypes.Internode() + TreeTypes.Meristem())

function prob_break(bud)
    # We move to parent node in the branch where the bud was created
    node =  parent(bud)
    # We count the number of internodes between node and the first Meristem
    # moving down the graph
    check, steps = has_descendant(node, condition = n -> data(n) isa TreeTypes.Meristem)
    steps = Int(ceil(steps/2)) # Because it will count both the nodes and the internodes
    # Compute probability of bud break and determine whether it happens
    if check
        prob =  min(1.0, steps*graph_data(bud).budbreak)
        return rand() < prob
    # If there is no meristem, an error happened since the model does not allow for this
    else
        error("No meristem found in branch")
    end
end
branch_rule = Rule(TreeTypes.Bud,
            lhs = prob_break,
            rhs = bud -> TreeTypes.BudNode() + TreeTypes.Internode() + TreeTypes.Meristem())

function create_tree(origin, growth, budbreak, orientation)
    axiom = T(origin) + RH(orientation) + TreeTypes.Internode() + TreeTypes.Meristem()
    tree =  Graph(axiom = axiom, rules = (meristem_rule, branch_rule),
                  data = TreeTypes.treeparams(growth = growth, budbreak = budbreak))
    return tree
end

getInternode = Query(TreeTypes.Internode)

function elongate!(tree, query)
    for x in apply(tree, query)
        x.length = x.length*(1.0 + data(tree).growth)
    end
end

function growth!(tree, query)
    elongate!(tree, query)
    rewrite!(tree)
end

function simulate(tree, query, nsteps)
    new_tree = deepcopy(tree)
    for i in 1:nsteps
        growth!(new_tree, query)
    end
    return new_tree
end
origins = [Vec(i,j,0) for i = 1:2.0:20.0, j = 1:2.0:20.0]
orientations = [rand()*360.0 for i = 1:2.0:20.0, j = 1:2.0:20.0]
growths = rand(LogNormal(-2, 0.3), 10, 10)
budbreaks = rand(Beta(2.0, 10), 10, 10)
forest = vec(create_tree.(origins, growths, budbreaks, orientations));
```

We run the simulation for a few steps to create a forest and add the soil:

```julia
newforest = [simulate(tree, getInternode, 15) for tree in forest];
scene = Scene(newforest);
soil = Rectangle(length = 21.0, width = 21.0)
rotatey!(soil, pi/2)
VirtualPlantLab.translate!(soil, Vec(0.0, 10.5, 0.0))
VirtualPlantLab.add!(scene, mesh = soil, color = RGB(1,1,0),
                material = Lambertian(τ = 0.0, ρ = 0.21))
#render(scene, backend = "web", resolution = (800, 600))
```

Unlike in the previous example, we can no longer run a single raytracer to
compute daily photosynthesis, because of its non-linear response to irradiance.
Instead, we need to compute photosynthesis at different time points during the
day and integrate the results (e.g., using a Gaussian quadrature rule). However,
this does not require more computation than the previous example if the calculations
are done carefully to avoid redundancies.

Firstly, we can create the bounding volume hierarchy and grid cloner around the
scene once for the whole day using the `accelerate()` function (normally this is called
by VPL internally):

```julia
settings = RTSettings(pkill = 0.8, maxiter = 3, nx = 5, ny = 5, dx = 20.0,
                          dy = 20.0, parallel = true)
acc_scene = accelerate(scene, settings = settings, acceleration = BVH,
                       rule = SAH{6}(1,20));
```

Then we compute the relative fraction of diffuse PAR that reaches each leaf (once
for the whole day):

```julia
get_leaves(tree) = apply(tree, Query(TreeTypes.Leaf))

function calculate_diffuse!(;scene, acc_scene, forest, lat = 52.0*π/180.0, DOY = 182)
    # Create the dome of diffuse light
    dome = sky(scene,
                  Idir = 0.0, # No direct solar radiation
                  Idif = 1.0, # In order to get relative values
                  nrays_dif = 1_000_000, # Total number of rays for diffuse solar radiation
                  sky_model = StandardSky, # Angular distribution of solar radiation
                  dome_method = equal_solid_angles, # Discretization of the sky dome
                  ntheta = 9, # Number of discretization steps in the zenith angle
                  nphi = 12) # Number of discretization steps in the azimuth angle
    # Ray trace the scene
    settings = RTSettings(pkill = 0.9, maxiter = 4, nx = 5, ny = 5, dx = 20.0,
                          dy = 20.0, parallel = true)
    # Because the acceleration was pre-computed, use direct RayTracer constructor
    rtobj = RayTracer(acc_scene, materials(scene), dome, settings);
    trace!(rtobj)
    # Transfer power to PARdif
    @threads for tree in forest
        for leaf in get_leaves(tree)
            leaf.PARdif = power(leaf.mat)[1]/(π*leaf.length*leaf.width/4)
        end
    end
    return nothing
end
```

Once the relative diffuse irradiance has been computed, we can loop over the
day and compute direct PAR by using a single ray tracer and update photosynthesis
from that. Notice that here we convert solar radiation to PAR in umol/m2/s as
opposed to W/m2 (using `:flux` rather than `:power` in the `waveband_conversion`
function):

```julia
function calculate_photosynthesis!(;scene, acc_scene, forest, lat = 52.0*π/180.0, DOY = 182,
                 f = 0.5, w = 0.5, DL = 12*3600)
    # Compute the solar irradiance assuming clear sky conditions
    Ig, Idir, Idif = clear_sky(lat = lat, DOY = DOY, f = f)
    # Conversion factors to PAR for direct and diffuse irradiance
    PARdir = Idir*waveband_conversion(Itype = :direct,  waveband = :PAR, mode = :flux)
    PARdif = Idif*waveband_conversion(Itype = :diffuse, waveband = :PAR, mode = :flux)
    # Create the light source for the ray tracer
    dome = sky(scene, Idir = PARdir, nrays_dir = 100_000, Idif = 0.0)
    # Ray trace the scene
    settings = RTSettings(pkill = 0.9, maxiter = 4, nx = 5, ny = 5, dx = 20.0,
                          dy = 20.0, parallel = true)
    rtobj = RayTracer(acc_scene, materials(scene), dome, settings)
    trace!(rtobj)
    # Transfer power to PARdif
    @threads for tree in forest
        ph = data(tree).photos
        for leaf in get_leaves(tree)
            leaf.PARdir = power(leaf.mat)[1]/(π*leaf.length*leaf.width/4)*PARdir
            leaf.PARdif = leaf.PARdif*PARdif
            PAR = leaf.PARdir + leaf.PARdif
            leaf.Ag += (photosynthesis(ph, PAR = PAR).A + ph.Rd25)*w*DL
        end
    end
    return nothing
end

# Reset photosynthesis
function reset_photosynthesis!(forest)
    @threads for tree in forest
        for leaf in get_leaves(tree)
            leaf.Ag = 0.0
        end
    end
    return nothing
end
```

This function may now be run for different time points during the day based on
a Gaussian quadrature rule:

```julia
function daily_photosynthesis(forest; DOY = 182, lat = 52.0*π/180.0)
    # Compute fraction of diffuse irradiance per leaf
    calculate_diffuse!(scene = scene, acc_scene = acc_scene, forest = forest,
                        DOY = DOY, lat = lat);
    # Gaussian quadrature over the
    NG = 5
    f, w = gausslegendre(NG)
    w ./= 2.0
    f .= (f .+ 1.0)/2.0
    # Reset photosynthesis
    reset_photosynthesis!(forest)
    # Loop over the day
    dec = declination(DOY)
    DL = day_length(lat, dec)*3600
    for i in 1:NG
        println("step $i out of $NG")
        calculate_photosynthesis!(scene = scene, acc_scene = acc_scene, forest = forest,
                                  f = f[i], w = w[i], DL = DL, DOY = DOY, lat = lat)
    end
end
```

And we scale to the tree level with a simple query:

```julia
function canopy_photosynthesis!(forest)
    # Integrate photosynthesis over the day at the leaf level
    daily_photosynthesis(forest)
    # Aggregate to the the tree level
    Ag = Float64[]
    for tree in forest
        data(tree).Ag = sum(leaf.Ag*π*leaf.length*leaf.width/4 for leaf in get_leaves(tree))
        push!(Ag, data(tree).Ag)
    end
    return Ag/1e6 # mol/tree
end

# Run the canopy photosynthesis model
Ag = canopy_photosynthesis!(newforest);
histogram(Ag)
```

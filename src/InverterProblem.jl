module InverterProblem

using ProgressMeter

####
#### Representation of an AndOr plane
####

struct AndOr{N}
    masks::NTuple{N,NTuple{N,Bool}}
end

AndOr{N}() where {N} = AndOr{N}(ntuple(_ -> ntuple(_ -> false, N), N))

evaluate(x::Bool, y::Bool) = ~y | (y & x)
function and(x::T, mask::T)::Bool where {T <: NTuple{N,Bool} where N}
    return any(isequal(true), mask) && reduce(&, evaluate.(x, mask))
end

# Precompile some 'and' methods
#
# It seems to be important to compile `and(NTuple{5,Bool},NTuple{5,Bool})` for it to inline
# properly into `(A::AndOr{N})`
for i in 1:5
    __t = ntuple(_ -> false, i)
    and(__t, __t)
end

# Broadcast the inputs `x` along each of the masks
# Make `x` a scalar so it is applied to each mask.
(A::AndOr{N})(x::NTuple{N,Bool}) where {N} = reduce(|, and.((x,), A.masks))
(A::AndOr)(x::Bool...) = A(x)

function bulkeval(f, x::Vector{NTuple{N,Bool}}) where {N}
    bitmask = 0
    for i in x
        bitmask <<= 1
        bitmask |= f(i)
    end
    return bitmask
end

# Pretty showing for AndOr
function Base.show(io::IO, A::AndOr{N}) where {N}
    num_nonempty = count(x -> any(isequal(true), x), A.masks)
    seen_nonempty = 0
    for mask in A.masks
        numentries = count(isequal(true), mask)
        if numentries > 0
            seen_nonempty += 1
            seen = 0
            print(io, "(")
            for (j, v) in enumerate(mask)
                if v == true
                    print(io, "x$j")
                    seen += 1
                    if seen < numentries
                        print(io, " ∘ ")
                    end
                end
            end
            print(io, ")")
            if seen_nonempty < num_nonempty
                print(io, " + ")
            end
        end
    end
end

#####
##### Misc helper functions
#####

gentuples(::Val{N}) where {N} = Iterators.product(ntuple(i -> (false, true), N)...) |> collect |> vec

#####
##### Logic for finding minimal functions
#####

function findminimal(::Val{N}) where {N}
    # Generate all inputs
    inputs = gentuples(Val{N}())

    # Create a mapping between output tuple collections and the AndOr that generated them
    fns = Dict{Int, AndOr{N}}()
    progress_meter = Progress(2 ^ (N * N))
    for mask in Iterators.product(ntuple(i -> inputs, N)...)
        f = AndOr(mask)
        get!(fns, bulkeval(f, inputs), f)
        next!(progress_meter)
    end
    return collect(values(fns))
end

#####
##### Search
#####

function search(;
        three_input = findminimal(Val{3}()),
        four_input = findminimal(Val{4}()),
        five_input = findminimal(Val{5}()),
    )

    # Generate the input values
    inputs = gentuples(Val{3}())

    # Generate the expected outputs for each variable
    x1_output = bulkeval(x -> ~x[1], inputs)
    x2_output = bulkeval(x -> ~x[2], inputs)
    x3_output = bulkeval(x -> ~x[3], inputs)

    progress_meter = Progress(length(three_input) * length(four_input))
    foundmask = Dict{Int,Union{Nothing,AndOr{5}}}(
        x1_output => nothing,
        x2_output => nothing,
        x3_output => nothing,
    )

    # Iterate through the `three_input` functions for the first inverter.
    # With that, iterate through the four input for the second inverter.
    #
    # Finally, try `five_input` for each of the output signals.
    # Abort if any of the `five_output` options fails
    for (f, g) in Iterators.product(three_input, four_input)
        # Clear mask
        for k in keys(foundmask)
            foundmask[k] = nothing
        end

        for h in five_input
            F = x -> begin
                f̄ = ~f(x)
                ḡ = ~g(x..., f̄)
                return h(x..., f̄, ḡ)
            end
            output = bulkeval(F, inputs)
            if haskey(foundmask, output)
                foundmask[output] = h
            end
        end
        next!(progress_meter)

        if !any(isnothing, values(foundmask))
            finish!(progress_meter)
            println("f = ", f)
            println("g = ", g)
            println("x̄1 = ", foundmask[x1_output])
            println("x̄2 = ", foundmask[x2_output])
            println("x̄3 = ", foundmask[x3_output])
            return nothing
        end
    end
    return nothing
end

end # module

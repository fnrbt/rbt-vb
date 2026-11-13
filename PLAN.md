# Correctness, structure, and performance remediation plan

## Scope and delivery contract

This plan covers every correctness finding from the repository review, plus the associated structure, persistence, test coverage, and performance work. The original findings came from static inspection. Implementation and regression verification are complete; results are recorded below.

- Implement the active pipeline: `Lexer.fs` → `ParserSimple.fs` → `Validator.fs` → `Compiler.fs` → `StackVm.fs`, including API, builtins, and bytecode persistence.
- Preserve VBScript/VBA semantics rather than special-casing the review examples.
- Every correctness item gets a deterministic regression asserting a consumer-visible result, error, or side effect. Use the public API wherever possible; inspect VM state only for stack/frame invariants.
- Do not silently accept unsupported executable constructs. The listed features must be implemented end to end; any scope reduction requires explicit approval.
- Keep native and Fable behavior aligned. Avoid native-only helpers in the shared execution path.
- Coordinate AST, value, instruction, compiler, VM, and serializer changes. Migrate all callers; no compatibility shims or half-implemented representations.
- Run the narrowest relevant checks once after each completed slice. Run the integration matrix after the phases converge.

## Sequence and dependencies

1. Establish an assertion-based regression harness.
2. Fix complete-input parsing, logical lines, expression grammar, and dialect traversal.
3. Establish declaration binding and scope isolation.
4. Repair control-flow lowering.
5. Repair procedure, member, and property call contracts.
6. Implement statement-level, frame-local error recovery.
7. Complete array semantics and shared value semantics.
8. Validate persisted bytecode and finish structural cleanup.
9. Optimize allocations after semantic contracts stabilize.
10. Run integration checks and gate publishing.

Phases 2 and 3 can proceed independently after the harness. Phase 4 needs the parser and binding contracts. Phase 5 needs binding; phase 6 needs stable call/return contracts. Array and value work can be separate slices, but both touch the VM and require a single integration owner. Serialization follows the final representation changes; performance work follows correctness.

## Phase 1 — Regression infrastructure

Primary scope: a dedicated test project, `FsVb.fs`, existing `test*.vbs` / `test*.vba`, `.forge/actions.lua`, `.forge/publish`.

- [x] Add an isolated test harness that executes source through `runSource`, checks globals and host-observed side effects, and asserts parse/validation/runtime errors.
- [x] Keep stdout-dependent tests isolated; prefer a test host object for observable calls rather than globally redirecting console output.
- [x] Turn representative existing algorithm/class scripts into asserted regression cases. Keep benchmarks separate from correctness tests.
- [x] Record each review example as a targeted regression with an explicit expected VB result. Confirm syntax and dialect assumptions before attributing a failure to a particular layer.
- [x] Establish native test execution and a Fable consumer smoke scenario. Record any unavailable runtime as an explicit verification gap, not a pass.
- [x] Prepare a test command suitable for normal changes and publishing; activate the required publish gate in phase 10.

Acceptance: failures produce nonzero exit status and identify semantic mismatches without manual inspection of printed output.

## Phase 2 — Frontend correctness

Primary scope: `Lexer.fs`, `ParserSimple.fs`, `Ast.fs`, `Validator.fs`.

### C01 — Reject partially parsed programs

- [x] Require EOF in strict and tolerant parsing. Preserve the first meaningful failing diagnostic instead of treating all parser failures as successful list termination.
- [x] Distinguish a valid block terminator from an invalid statement; avoid masking malformed nested blocks through backtracking.
- [x] Assert that `x = 1\nDim\nx = 2` fails, valid trailing separators succeed, and unsupported trailing syntax cannot produce an empty or truncated successful program.

### C02 — Restore VB operator precedence

- [x] Rebuild expression levels for exponentiation, unary arithmetic, multiplication/division, integer division, Mod, addition/subtraction, concatenation, comparisons, Not, And, Or, Xor, Eqv, and Imp.
- [x] Encode VB associativity explicitly, including chained exponentiation; check against authoritative language semantics rather than assuming mathematical conventions.
- [x] Assert `2 * 3 ^ 2 = 18`, unary/power interactions, concatenation versus addition, Not versus comparisons, and mixed logical operators. Include parentheses overriding precedence.

### C04 — Keep complete inline If branches conditional

- [x] Consume colon-separated statements through the logical end of line or the appropriate Else boundary. Preserve nested inline-If ownership of Else.
- [x] Assert that `If False Then x = 1 : x = 2` leaves the prior value unchanged, and exercise multi-statement Then and Else branches.

### C05 — Represent unconditional Do loops correctly

- [x] Represent conditionless `Do … Loop` as unconditional repetition rather than a false postcondition.
- [x] Assert repetition until `Exit Do`, plus pre-tested and post-tested While/Until behavior. Use a bounded scenario so a regression cannot hang the suite.

### C11 — Recognize implicit procedure calls consistently

- [x] Preserve argumentless statement-position calls such as `Ping` and member calls without confusing them with variable reads.
- [x] Reuse the ordinary non-block statement grammar inside inline If, including implicit calls with arguments. Let binding resolve statement-position ambiguities where necessary.
- [x] Assert bare procedure/member calls and `If True Then WScript.Echo "ok"`, with false-branch cases proving absence of side effects.

### C12 — Preserve logical lines across continuation markers

- [x] Consume valid continuation whitespace/newline without marking the following token as beginning a new logical statement.
- [x] Retain physical source coordinates for diagnostics and reject invalid continuation forms according to the selected dialect.
- [x] Assert `WScript.Echo _\n "ok"` invokes the host once, including continuation within expressions and inline branches.

### C25 — Traverse expressions and case tests during validation

- [x] Add recursive expression and case-test validation throughout statements, declarations, defaults, and nested bodies.
- [x] Reject VBA-only expression/case forms in VBScript, including TypeOf and Case ranges/comparisons, in both strict and tolerant-plus-validation flows.
- [x] Assert equivalent supported VBA input remains valid and errors identify the offending construct.

Acceptance: malformed input never runs a prefix; each grammar regression reaches the intended AST and public execution result.

## Phase 3 — Declaration binding and scope isolation

Primary scope: `Compiler.fs`, relevant AST/signature metadata.

### C06 — Register declarations before compiling bodies

- [x] Register procedure identities, class identities, member signatures, parameter passing modes, and return slots before lowering any body.
- [x] Keep function/class indices stable while compiling implementations. Recursive calls must see the complete signature, including ByRef modes.
- [x] Resolve known user procedures before builtin fallback; unresolved calls must not silently become successful no-ops.
- [x] Assert forward calls, mutual recursion, recursive ByRef forwarding, and New of a class from its own methods or earlier declarations.

### C07 — Isolate class and procedure compilation contexts

- [x] Save and restore class-field context around whole class compilation, not just around individual method bodies.
- [x] Centralize scope resolution used by reads, writes, loop variables, and reference creation. Preserve local shadowing and host-global behavior.
- [x] Assert a top-level variable and an ordinary procedure local can share a class field's name without being redirected to an implicit receiver. Cover multiple successive classes.

Acceptance: declaration order does not change callable behavior; class compilation cannot alter unrelated variable resolution.

## Phase 4 — Control-flow lowering

Primary scope: `Compiler.fs`, shared scope resolver.

### C03 — Preserve ElseIf false-branch chains

- [x] Patch each preceding conditional's false jump exactly once; collect independent jumps to the common end.
- [x] Assert two or more ElseIf clauses, with and without Else, exercising first/middle/last matches and no match.

### C13 — Evaluate For-loop direction at runtime

- [x] Evaluate and store the end and step values at loop entry according to VB semantics. Choose termination comparison from the evaluated step's sign, not AST shape.
- [x] Assert variable/computed negative steps, positive and fractional steps, initially empty ranges, and side-effecting bound/step expressions evaluated the correct number of times.

### C14 — Store For Each variables in the resolved scope

- [x] Use the shared assignment resolver for the iteration variable rather than unconditionally allocating a local.
- [x] Assert declared globals, procedure locals, and shadowed names observe each iteration value.

### C15 — Allocate unique For Each temporaries

- [x] Allocate compiler-owned collection/index slots per loop, inaccessible through user identifiers and independent across nested loops.
- [x] Assert a two-by-two nested iteration executes four bodies and that inner-loop exit does not corrupt outer state.

Acceptance: loop state and branch targets remain correct under nesting and mixed scope.

## Phase 5 — Call frames, members, and properties

Primary scope: `Ast.fs`, `Values.fs`, `Bytecode.fs`, `Compiler.fs`, `StackVm.fs`.

### C08 — Give property assignments a zero-result contract

- [x] Define call-site result disposition explicitly: expression result, discarded result, or constructor result. Preserve it through frame return.
- [x] Ensure setters leave the operand stack at its statement baseline without consuming unrelated operands.
- [x] Assert repeated Property Let assignments beyond the old stack limit complete with correct state; include nested getter/setter calls and constructor execution.

### C17 — Preserve method ByRef semantics

- [x] Carry assignable argument locations into dynamic method dispatch and apply the resolved method's parameter modes. Do not pass references indiscriminately to ByVal parameters or host calls.
- [x] Preserve VB rules for expression/parenthesized arguments and reference forwarding. Coordinate array-element locations with phase 7.
- [x] Assert a ByRef method updates its caller, ByVal does not, and a reference forwarded through another call remains attached to its original storage.

### C18 — Dispatch Property Set separately from Property Let

- [x] Preserve Let versus Set in member-assignment AST and bytecode; dispatch the corresponding class handler.
- [x] Update all parser/compiler/VM consumers together, including direct fields and host assignment behavior.
- [x] Assert object assignment invokes Property Set, scalar assignment invokes Property Let, and getters observe the stored values.

### C19 — Make script-member lookup case-insensitive

- [x] Normalize script-owned field, method, property, and initializer keys consistently at declaration and lookup boundaries, including Fable.
- [x] Do not force script normalization onto host-object APIs that receive member spellings.
- [x] Assert mixed-case reads/writes/calls and lowercase `class_initialize` behave identically to declared casing.

Acceptance: calls have explicit stack effects, receiver identity, passing modes, and return disposition across normal and nested execution.

## Phase 6 — Error handling

Primary scope: statement/source metadata in `Ast.fs` / `Bytecode.fs`, `Compiler.fs`, frames and dispatch in `StackVm.fs`.

### C09 — Recover at statement boundaries

- [x] Emit real statement continuation metadata and operand-stack baselines. Source line numbers alone are insufficient because colon-separated statements share a line.
- [x] On Resume Next, discard partial expression state and skip the failed statement without executing its remaining assignment/call instructions.
- [x] Assert `x = 7` followed by a handled `x = 1 / 0` leaves x unchanged and executes the next statement. Cover nested calls, same-line statements, and repeated failures without stack growth.

### C10 — Scope handlers to procedure frames

- [x] Store handler mode and active-handler state per frame. Initialize and restore them on calls/returns.
- [x] Propagate unhandled errors by unwinding frames to the appropriate caller handler, restoring operand and receiver state.
- [x] Assert a callee's Resume Next does not survive its return, a caller handler catches a callee failure at the proper continuation, and active handlers do not re-enter themselves incorrectly.

### C20 — Route Err.Raise through common error dispatch

- [x] Send explicit errors through the same dispatch/unwind mechanism as runtime failures, preserving the supplied error number and description.
- [x] Assert GoTo transfer without fallthrough, Resume Next continuation, unhandled API errors, and Err clearing/handler disabling behavior.

Acceptance: recovery never resumes inside a partially executed statement or with another procedure's locals/handler state.

## Phase 7 — Arrays and shared value semantics

### C16 — Preserve all array dimensions and indices

Primary scope: `Values.fs`, `Ast.fs`, `Bytecode.fs`, `Compiler.fs`, `StackVm.fs`, `Builtins.fs`.

- [x] Define array rank, bounds, and storage/indexing consistently. Allocate every declared dimension and use every supplied index for reads and writes.
- [x] Update declaration, ReDim/Preserve, array calls, iteration, UBound/LBound, and array-element references together. Enforce dimensional/bounds errors rather than returning Empty or ignoring writes.
- [x] Preserve one-dimensional builtin Array behavior and host/runtime consumers; migrate every VArray caller to the chosen representation.
- [x] Assert independent two-dimensional cells, rank/index errors, declared bounds, dimension-specific UBound/LBound, and valid/invalid Preserve operations.

### C21 — Preserve numeric precision during coercion

Primary scope: a shared semantic conversion layer used by `Builtins.fs` and `StackVm.fs`.

- [x] Replace integer-only fallback with VB-aware coercion that preserves floating-point operands. Define Empty, Boolean, Null, numeric-string, and invalid-conversion behavior explicitly.
- [x] Assert `2.5 * "2" = 5`, `2.5 + Empty = 2.5`, and relevant Boolean/Null/error cases without silently converting invalid values to zero.

### C22 — Implement string relational comparisons

- [x] Route string ordering through the appropriate VB comparison semantics, consistent across relational opcodes and Select Case ranges.
- [x] Assert `"a" < "b"`, `"a" <= "a"`, reverse comparisons, and the applicable binary/text comparison modes.

### C23 — Correct numeric builtin conversion and rounding

- [x] Separate CInt/CLng rounding, Int flooring, Fix truncation, and Round ties-to-even. Honor each conversion's range/error behavior rather than sharing a truncating helper.
- [x] Assert positive/negative fractions, midpoint ties, and overflow edges. Concrete examples: `CInt(1.5) = 2`, `Int(-1.5) = -2`, `Fix(-1.5) = -1`, `Round(2.5) = 2`.

### C24 — Correct string builtin arguments and boundaries

- [x] Honor StrComp's comparison argument and applicable default instead of always ignoring case.
- [x] Return zero for InStr starts beyond the searchable string; distinguish valid no-match cases from invalid arguments. Honor its comparison argument through the same comparison policy.
- [x] Assert binary versus text comparison, explicit/default arguments, empty strings, and starts at/beyond the end.

Acceptance: VM operators and builtins use one conversion/comparison policy rather than diverging helpers.

## Phase 8 — Persistence and structural cleanup

Primary scope: `BytecodeIO.fs`, `Bytecode.fs`, `Rbt.Vb.fsproj`, API consumers.

- [x] Update serialization for every changed instruction, frame requirement, array constant policy, and statement metadata field. Bump the format version when incompatible; reject incompatible old blobs explicitly.
- [x] Treat externally supplied bytecode as untrusted: validate encoded counts against remaining input and resource limits before allocating; validate function/class references, jump targets, local/global/constant slots, parameter metadata, and statement continuations before execution.
- [x] Define and enforce the bytecode validation boundary for both deserialized and directly supplied programs. Do not describe validation as a sandbox for arbitrary script execution.
- [x] Assert valid round-trip execution and deterministic rejection of truncated, oversized-count, invalid-reference, invalid-target, and incompatible-version blobs without huge allocations.
- [x] Keep the existing frontend/compiler/VM separation; extract shared binding and value semantics only where they remove duplicated policy.
- [x] Resolve the status of excluded `Parser.fs` and `Vm.fs`. Before deleting unrelated legacy implementations, obtain approval; otherwise clearly mark them as inactive historical code.
- [x] Update project compile ordering, Fable packaging, and all public API consumers affected by representation changes. Use symbol references where language-server support is available.
- [x] Document supported behavior and the resulting compatibility changes; retain real source locations in diagnostics rather than all-zero instruction lines.

Acceptance: persisted and directly compiled execution share the same invariants, and inactive code cannot be mistaken for the shipping implementation.

## Phase 9 — Performance after correctness

Primary scope: `StackVm.fs`, call lowering, existing `bench_*.vba` programs.

- [x] Remove the temporary argument array from script calls by populating the new frame directly in the correct argument order. Retain arrays where the host interface requires them.
- [x] Clear inactive operand/frame slots after extracting values. Coordinate with completed error-recovery semantics so recovery never depends on stale backing-array contents.
- [x] Replace oversized unconditional operand/call-stack allocations with conservative growable storage or compiler-derived sizing, retaining explicit overflow/resource behavior.
- [x] Measure cold/tiny execution allocations separately from sustained recursion, object/property calls, strings, arrays, and numeric loops. Compare only programs whose correctness assertions pass.
- [x] Add retention verification for objects reachable only through popped slots; keep GC-sensitive measurements out of deterministic semantic tests.
- [x] Avoid pooling or broader VM redesign unless measurements justify the complexity.

Acceptance: measured allocation/retention improvements without changes to output, error behavior, argument evaluation, or stack discipline.

## Phase 10 — Integration and release gate

- [x] Run the regression matrix for every C01–C25 item in its applicable dialects, including strict and tolerant-plus-validation parsing where relevant.
- [x] Build both library target frameworks; execute native regressions on the runnable target and exercise a real Fable consumer for shared-runtime changes.
- [x] Run representative existing algorithm/class scripts with asserted outcomes and the CLI success/error paths with correct exit codes.
- [x] Verify serialize/deserialize execution parity after all bytecode changes land.
- [x] Make the assertion-based suite a required publishing prerequisite and run it on ordinary changes through the available Forge action mechanism.
- [x] Update compatibility documentation/changelog and remove temporary investigation scripts.
- [x] Report actual commands/results, measured performance changes, and any environmental verification gaps. No inferred execution claims.

## Completion checklist

- [x] All 25 correctness items are implemented, not merely diagnosed or rejected as unsupported.
- [x] Every correctness item has a regression exercising its consumer-visible failure mode.
- [x] AST, values, bytecode, compiler, VM, serializer, API, and Fable consumers agree on changed contracts.
- [x] No silent partial parses, lost calls, leaked receiver contexts, stack leaks, or cross-frame handlers remain on covered paths.
- [x] Persistence has explicit validation/versioning and publishing has an automated correctness gate.
- [x] Structural cleanup and measured allocation improvements are complete, with any legacy deletion explicitly authorized.


## Implementation verification

- Native: all 60 public-API regression groups, the shared semantic suite, persistence rejection/round-trip cases, and the existing algorithm/class fixtures pass via `dotnet run --project tests/Rbt.Vb.Tests.fsproj`.
- Fable 5.27.0 / Node 24: all 60 public-API groups and the shared semantic suite pass. Persistence and disk-based fixture loading remain native-only by design.
- Both `net10.0` and `netstandard2.0` compile. The explicit netstandard build completed with zero warnings/errors.
- CLI prints 18 for `2 * 3 ^ 2`; malformed trailing declarations return nonzero instead of running a prefix.
- Publishing script syntax checked; publishing itself was not invoked. The native regression command now gates package creation/upload, and ordinary Forge changes select the test action.
- Bytecode format is version 2. The AST, array/reference values, member-assignment instructions, comparison context, and statement/frame contracts changed together.
- Historical Parser.fs and Vm.fs were retained and marked inactive; no legacy deletion was performed.

### Allocation measurements

Measurements use the pre-existing Debug assembly versus the updated Debug assembly, with precompiled programs and output assertions. These are short diagnostic runs, not statistically controlled throughput benchmarks. Correctness fixes add validation, statement tracking, and reference semantics, so reduced allocation does not imply faster execution in every workload.

| Workload | Before bytes/execution | After bytes/execution | Before ms/execution | After ms/execution |
| --- | ---: | ---: | ---: | ---: |
| Tiny assignment | 559819 | 1776 | 0.6310 | 0.0051 |
| Recursive Fibonacci | 732649 | 130985 | 1.8927 | 2.2108 |
| Object field loop | 704161 | 316857 | 3.3045 | 5.5931 |
| Array loop | 591497 | 48881 | 0.8212 | 0.8178 |
| String loop | 640377 | 104633 | 1.0425 | 1.1475 |

A separate WeakReference measurement confirmed arrays in popped operand slots and returned procedure frames are collectible while the VM state remains alive. The measurement scripts were temporary and removed after recording these results.

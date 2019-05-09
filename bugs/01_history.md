- Duplicate type aliases

- Add a package

- Array
```
entries: Location<HistoryLocationState>[];

var entries: Location<HistoryLocationState> []
```

- Extends
```
export interface MemoryHistory<HistoryLocationState = LocationState> extends History<HistoryLocationState> {

external interface MemoryHistory<HistoryLocationState extends LocationState> : History<HistoryLocationState> {
```

- Missing type arguments
```
export interface MemoryHistory<HistoryLocationState = LocationState> extends History<HistoryLocationState> {
export default function createMemoryHistory(options?: MemoryHistoryBuildOptions): MemoryHistory;

external interface MemoryHistory<HistoryLocationState extends LocationState> : History<HistoryLocationState> {
external fun createMemoryHistory(options: MemoryHistoryBuildOptions?): MemoryHistory = definedExternally
```

- Type alias cannot by dynamic

```
typealias Prompt = dynamic
```

- Type aliases in separate file (typeskt.kt)

- Skip '?' for `dynamic?`

- DSL: don't use `open var`!!! (generates a backing property or something like that)
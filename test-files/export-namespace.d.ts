export namespace History {
    export type LocationDescriptor<S = LocationState> = Path | LocationDescriptorObject<S>;
    export type LocationKey = string;
    export type LocationListener = (location: Location, action: Action) => void;
    export type LocationState = any;
    export type Path = string;
    export type Pathname = string;
    export type Search = string;
    export type TransitionHook = (location: Location, callback: (result: any) => void) => any;
    export type TransitionPromptHook = (location: Location, action: Action) => string | false | void;
    export type Hash = string;
    export type Href = string;
}

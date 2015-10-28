(** This function fills in the [aliases] and [version] fields of the given
 * trace file. Note that this requires the arguments facts to be filled in. *)
val calculate_versions: LocalFacts.arguments_tracefile -> LocalFacts.facts_tracefile

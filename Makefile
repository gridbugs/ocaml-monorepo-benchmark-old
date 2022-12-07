package_stats:
	dune exec --display=quiet bin/package_stats.exe

dependency_closure_sexp_arm64:
	mkdir -p dist
	dune exec --display=quiet bin/dependency_closure_sexp.exe arm64 > dist/packages.sexp

dependency_closure_sexp_x86_64:
	mkdir -p dist
	dune exec --display=quiet bin/dependency_closure_sexp.exe x86_64 > dist/packages.sexp

clean:
	dune clean
	rm -rf dist

.PHONY: package_stats dependency_closure clean

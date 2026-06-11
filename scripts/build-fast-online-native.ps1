$ErrorActionPreference = "Stop"

$repo = Resolve-Path (Join-Path $PSScriptRoot "..")
Set-Location $repo

$r = Get-Command R.exe -ErrorAction SilentlyContinue
if ($null -eq $r) {
  $defaultR = "C:\Program Files\R\R-4.5.1\bin\R.exe"
  if (Test-Path $defaultR) {
    $rPath = $defaultR
  } else {
    throw "R was not found on PATH and $defaultR does not exist."
  }
} else {
  $rPath = $r.Source
}

$candidateRtools = @(
  (Join-Path $env:USERPROFILE "rtools45"),
  "C:\rtools45",
  "C:\Rtools45"
)
foreach ($rtools in $candidateRtools) {
  $usrBin = Join-Path $rtools "usr\bin"
  $compilerBin = Join-Path $rtools "x86_64-w64-mingw32.static.posix\bin"
  if ((Test-Path (Join-Path $usrBin "make.exe")) -and
      -not (($env:PATH -split ";") -contains $usrBin)) {
    $env:PATH = "$usrBin;$env:PATH"
  }
  if ((Test-Path (Join-Path $compilerBin "gcc.exe")) -and
      -not (($env:PATH -split ";") -contains $compilerBin)) {
    $env:PATH = "$compilerBin;$env:PATH"
  }
  if (Test-Path (Join-Path $compilerBin "gcc.exe")) {
    $env:RTOOLS45_HOME = $rtools
    $env:LOCAL_SOFT = $compilerBin
    break
  }
}

$make = Get-Command make -ErrorAction SilentlyContinue
if ($null -eq $make) {
  throw "make was not found on PATH or in the standard/user Rtools locations."
}

Write-Host "Using R: $rPath"
Write-Host "Using make: $($make.Source)"

& $rPath CMD INSTALL --no-multiarch --with-keep.source .
if ($LASTEXITCODE -ne 0) {
  throw "R CMD INSTALL failed."
}

Rscript scripts\validate-fast-online-simple-adaptive.R --optim=false --require-native=true
if ($LASTEXITCODE -ne 0) {
  throw "Native validation failed."
}

Rscript scripts\benchmark-fast-online-simple-adaptive.R --optim-maxit=2
if ($LASTEXITCODE -ne 0) {
  throw "Native benchmark failed."
}

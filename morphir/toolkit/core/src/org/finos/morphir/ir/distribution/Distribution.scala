package org.finos.morphir.ir.distribution

import org.finos.morphir.ir.Module.{QualifiedModuleName, Specification => ModSpec}
import org.finos.morphir.ir.PackageModule.{
  Definition => PackageDefinition,
  PackageName,
  USpecification => UPackageSpecification
}
import org.finos.morphir.ir.Type.{USpecification => UTypeSpec}
import org.finos.morphir.ir.Type.Specification.TypeAliasSpecification
import org.finos.morphir.ir.Type.Type.Reference
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.{USpecification => UValueSpec, Definition => ValueDefinition}
import org.finos.morphir.ir.{FQName, Name, QName}

sealed trait Distribution
object Distribution {
  final case class Library(
      packageName: PackageName,
      dependencies: Map[PackageName, UPackageSpecification],
      packageDef: PackageDefinition.Typed
  ) extends Distribution { self =>

    // Look up a module specification by package and module path in a distribution.
    def lookupModuleSpecification(packageName: PackageName, module: QualifiedModuleName): Option[ModSpec.Raw] =
      self match {
        case Library(`packageName`, _, packageDef) =>
          packageDef.toSpecification.modules.get(module)
        case Library(_, dependencies, _) =>
          dependencies.get(packageName).flatMap(_.lookupModuleSpecification(module.toPath))
      }

    // Look up a type specification by package, module and local name in a distribution.
    def lookupTypeSpecification(pName: PackageName, module: QualifiedModuleName, localName: Name): Option[UTypeSpec] =
      lookupModuleSpecification(pName, module).flatMap(_.lookupTypeSpecification(localName))

    // Look up the base type name following aliases by package, module and local name in a distribution.
    def lookupBaseTypeName(fqName: FQName): Option[FQName] =
      lookupModuleSpecification(fqName.packagePath, fqName.getModuleName).flatMap(modSpec =>
        modSpec
          .lookupTypeSpecification(fqName.localName)
          .flatMap(typeSpec =>
            typeSpec match {
              case TypeAliasSpecification(_, Reference(_, aliasFQName, _)) => lookupBaseTypeName(aliasFQName)
              case _                                                       => Some(fqName)
            }
          )
      )

    // Look up a value specification by package, module and local name in a distribution.
    def lookupValueSpecification(
        packageName: PackageName,
        module: QualifiedModuleName,
        localName: Name
    ): Option[UValueSpec] =
      lookupModuleSpecification(packageName, module).flatMap(_.lookupValueSpecification(localName))

    // Look up a value definition by qualified name in a distribution. The value will only be searched in the current
    // package.
    def lookupValueDefinition(fqName: FQName): Option[ValueDefinition[scala.Unit, UType]] =
      self match {
        case Library(pName, _, packageDef) if pName == fqName.packagePath =>
          packageDef.lookupModuleDefinition(fqName.modulePath).flatMap(_.lookupValueDefinition(fqName.localName))
        case Library(_, _, _) => None
      }

    // Get the package specification of a distribution.
    def lookupPackageSpecification: UPackageSpecification = packageDef.toSpecificationWithPrivate.eraseAttributes

    // Get the package name of a distribution.
    @inline def lookupPackageName: PackageName = packageName

    // Add a package specification as a dependency of this library.
    def insertDependency(
        dependencyPackageName: PackageName,
        dependencyPackageSpec: UPackageSpecification
    ): Distribution = Library(packageName, dependencies + (dependencyPackageName -> dependencyPackageSpec), packageDef)

    // Get all type specifications.
    def typeSpecifications: Map[FQName, UTypeSpec] = ???

    private def typeSpecsInDependencies: Map[FQName, UTypeSpec] = dependencies.map { case (pName, pSpec) =>
      pSpec.modules.map { case (mName, mSpec) =>
        mSpec.types.map { case (tName, documentedTypeSpec) =>
          (FQName(pName, mName, tName), documentedTypeSpec.value)
        }
      }
    }

  }
}

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

    def lookupValueSpecification(
        packageName: PackageName,
        module: QualifiedModuleName,
        localName: Name
    ): Option[UValueSpec] =
      lookupModuleSpecification(packageName, module).flatMap(_.lookupValueSpecification(localName))

    def lookupValueDefinition(qName: QName): Option[ValueDefinition[scala.Unit, UType]] =
      packageDef.lookupModuleDefinition(qName.modulePath).flatMap(_.lookupValueDefinition(qName.localName))

    def lookupPackageSpecification: UPackageSpecification = packageDef.toSpecificationWithPrivate.eraseAttributes

    @inline def lookupPackageName: PackageName = packageName

    def insertDependency(
        dependencyPackageName: PackageName,
        dependencyPackageSpec: UPackageSpecification
    ): Distribution = Library(packageName, dependencies + (dependencyPackageName -> dependencyPackageSpec), packageDef)
  }
}

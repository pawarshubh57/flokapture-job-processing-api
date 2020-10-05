using System;
using System.Runtime.InteropServices;
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.EntityRepositories;
using BusinessLayer.Models;

namespace CodeVortoJobQueueProcessingApi.CodeVortoServices
{
    public sealed class CodeVortoService : ICodeVortoService
    {
        private IntPtr _nativeResource = Marshal.AllocHGlobal(100);

        private BaseRepository<UserMaster> _userMaster;
        private BaseRepository<ProjectMaster> _projectMaster;
        private BaseRepository<FileTypeExtensionReference> _fileTypeExtensionReference;
        private BaseRepository<FileMaster> _fileMaster;
        private BaseRepository<PrimaryLanguageReference> _primaryCommandReference;
        private BaseRepository<StatementReferenceMaster> _statementReferenceMaster;
        private BaseRepository<BaseCommandReference> _baseCommandReference;
        private BaseRepository<ActionWorkflows> _actionWorkflowsRepository;
        private BaseRepository<FileTypeReference> _fileTypeReference;
        private BaseRepository<ConnectivityStepReference> _connectivityStepReferenceRepository;
        private BaseRepository<ConnectivityLinkReferece> _connectivityLinkRefereceRepository;
        private BaseRepository<DecisionChart> _decisionChartRepository;
        private BaseRepository<DataDependency> _dataDependencyRepository;
        private BaseRepository<ViewSourceMaster> _viewSourceMasterRepository;
        private BaseRepository<WorkflowReferences> _workflowReferences;
        private BaseRepository<WorkflowTreeviewSecondTabDetails> _workflowTreeviewSecondTabDetails;
        private BaseRepository<WorkflowTreeviewTabFirstDetails> _workflowTreeviewTabFirstDetails;
        private BaseRepository<SecondTabProgramDetails> _secondTabProgramDetails;
        private BaseRepository<FirstTabProgramDetails> _firstTabProgramDetails;
        private BaseRepository<RegexPatternMaster> _regexMaster;
        private BaseRepository<ActionWorkflowDetails> _actionWorkflowDetails;
        private BaseRepository<SolutionMaster> _solutionMaster;
        private BaseRepository<ProductConfiguration> _productConfiguration;
        private BaseRepository<SecondTabSubRoutineProgramDetails> _secondTabSubRoutineProgramDetails;
        private BaseRepository<DbCrudActivity> _dbCrudActivity;
        private BaseRepository<StatementWithDataDictionary> _statementWithDataDictionary;
        private BaseRepository<DataDictionary> _dataDictionary;
        private AppDbContext _appDbContext;
        private BaseRepository<LanguageKeyword> _languageKeywordRepository;
        private BaseRepository<CobolVariable> _cobolVaribaleRepository;
        private BaseRepository<UniverseDescriptor> _universeDescriptorRepository;
        private BaseRepository<DataInventory> _dataInventoryRepository;
        private BaseRepository<MissingObjects> _missingObjectRepository;
        private BaseRepository<CobolSection> _cobolSectionRepository;
        private BaseRepository<CrudObjectReferences> _crudObjectReferenceRepository;
        private BaseRepository<ProjectProcessingStep> _projectProcessingStepRepository;


        public BaseRepository<ProjectProcessingStep> ProjectProcessingStepRepository
        {
            get
            {
                return _projectProcessingStepRepository ?? (_projectProcessingStepRepository =
                           new ProjectProcessingStepRepository(new AppDbContext()));
            }
             
        }

        public BaseRepository<CrudObjectReferences> CrudObjectReferenceRepository
        {
            get
            {
                return _crudObjectReferenceRepository ?? (_crudObjectReferenceRepository =
                           new CrudObjectReferenceRepository(new AppDbContext()));
            }
        }

        public BaseRepository<CobolSection> CobolSectionRepository
        {
            get
            {
                return _cobolSectionRepository ?? (_cobolSectionRepository =
                           new CobolSectionRepository(new AppDbContext()));
            }
        }

        public BaseRepository<MissingObjects> MissingObjectsRepository
        {
            get
            {
                return _missingObjectRepository ?? (_missingObjectRepository =
                           new MissingObjectsRepository(new AppDbContext()));
            }
        }
        public BaseRepository<DataInventory> DataInventoryRepository
        {
            get
            {
                return _dataInventoryRepository ??
                       (_dataInventoryRepository = new DataInventoryRepository(new AppDbContext()));
            }
        }

        public BaseRepository<UniverseDescriptor> UniverseDescriptorRepository
        {
            get
            {
                return _universeDescriptorRepository ?? (_universeDescriptorRepository =
                           new UniverseDescriptorRepository(new AppDbContext()));
            }
        }
        public BaseRepository<CobolVariable> CobolVariableRepository
        {
            get
            {
                return _cobolVaribaleRepository ??
                       (_cobolVaribaleRepository = new CobolVariableRepository(new AppDbContext()));
            }
        }
        public AppDbContext AppDbContextRepository
        {
            get { return _appDbContext ?? (_appDbContext = new AppDbContext()); }
        }

        public BaseRepository<WorkflowTreeviewTabFirstDetails> WorkflowTreeviewTabFirstDetailsRepository
        {
            get
            {
                return _workflowTreeviewTabFirstDetails ??
                       (_workflowTreeviewTabFirstDetails =
                           new WorkflowTreeviewTabFirstDetailsRepository(new AppDbContext()));
            }
        }

        public BaseRepository<LanguageKeyword> LanguageKeywordRepository
        {
            get
            {
                return _languageKeywordRepository ??
                       (_languageKeywordRepository = new LanguageKeywordRepository(new AppDbContext()));
            }
        }
        public BaseRepository<DbCrudActivity> DbCrudActivityRepository
        {
            get { return _dbCrudActivity ?? (_dbCrudActivity = new DbCrudActivityRepository(new AppDbContext())); }

        }

        public BaseRepository<StatementWithDataDictionary> StatementWithDataDictionaryRepository
        {
            get
            {
                return _statementWithDataDictionary ??
                       (_statementWithDataDictionary = new StatementWithDataDictionaryRepository(new AppDbContext()));
            }
        }

        public BaseRepository<DataDictionary> DataDictionaryRepository
        {
            get { return _dataDictionary ?? (_dataDictionary = new DataDictionaryRepository(new AppDbContext())); }
        }

        public BaseRepository<ActionWorkflowDetails> ActionWorkflowDetailsRepository
        {
            get { return _actionWorkflowDetails ?? (_actionWorkflowDetails = new ActionWorkflowDetailsRepository(new AppDbContext())); }
        }

        public BaseRepository<SolutionMaster> SolutionMasterRepository
        {
            get { return _solutionMaster ?? (_solutionMaster = new SolutionMasterRepository(new AppDbContext())); }
        }

        public BaseRepository<ProductConfiguration> ProductConfigurationRepository
        {
            get { return _productConfiguration ?? (_productConfiguration = new ProductConfigurationRepository(new AppDbContext())); }
        }

        public BaseRepository<SecondTabSubRoutineProgramDetails> SecondTabSubRoutineProgramRepository
        {
            get
            {
                return _secondTabSubRoutineProgramDetails ??
                       (_secondTabSubRoutineProgramDetails = new SecondTabSubRoutineProgramDetailsRepository(new AppDbContext()));
            }
        }

        public BaseRepository<RegexPatternMaster> RegexPatternMasterRepository
        {
            get { return _regexMaster ?? (_regexMaster = new RegexPatternMasterRepository(new AppDbContext())); }
        }

        public BaseRepository<FirstTabProgramDetails> FirstTabProgramDetailsRepository
        {
            get { return _firstTabProgramDetails ?? (_firstTabProgramDetails = new FirstTabProgramDetailsRepository(new AppDbContext())); }
        }

        public BaseRepository<SecondTabProgramDetails> SecondTabProgramDetailsRepository
        {
            get { return _secondTabProgramDetails ?? (_secondTabProgramDetails = new SecondTabProgramDetailsRepository(new AppDbContext())); }
        }

        public BaseRepository<UserMaster> UserMasterRepository
        {
            get { return _userMaster ?? (_userMaster = new UserMasterRepository(new AppDbContext())); }
        }

        public BaseRepository<ProjectMaster> ProjectMasterRepository
        {
            get { return _projectMaster ?? (_projectMaster = new ProjectMasterRepository(new AppDbContext())); }
        }

        public BaseRepository<FileTypeExtensionReference> FileTypeExtensionRepository
        {
            get
            {
                return _fileTypeExtensionReference ??
                       (_fileTypeExtensionReference = new FileTypeExtensionReferenceRepository(new AppDbContext()));
            }
        }

        public BaseRepository<FileMaster> FileMasterRepository
        {
            get { return _fileMaster ?? (_fileMaster = new FileMasterRepository(new AppDbContext())); }
        }

        public BaseRepository<FileTypeReference> FileTypeReferenceRepository
        {
            get
            {
                return _fileTypeReference ?? (_fileTypeReference = new FileTypeReferenceRepository(new AppDbContext()));
            }
        }

        public BaseRepository<PrimaryLanguageReference> PrimaryLanguageReferenceRepository
        {
            get
            {
                return _primaryCommandReference ??
                       (_primaryCommandReference = new PrimaryLanguageReferenceRepository(new AppDbContext()));
            }
        }

        public BaseRepository<StatementReferenceMaster> StatementReferenceMasterRepository
        {
            get
            {
                return _statementReferenceMaster ??
                       (_statementReferenceMaster = new StatementReferenceRepository(new AppDbContext()));
            }
        }

        public BaseRepository<BaseCommandReference> BaseCommandReferenceRepository
        {
            get
            {
                return _baseCommandReference ??
                       (_baseCommandReference = new BaseCommandReferenceRepository(new AppDbContext()));
            }
        }

        public BaseRepository<ActionWorkflows> ActionWorkflowsRepository
        {
            get
            {
                return _actionWorkflowsRepository ??
                       (_actionWorkflowsRepository = new ActionWorkflowsRepository(new AppDbContext()));
            }
        }

        public BaseRepository<ConnectivityStepReference> ConnectivityStepReferenceRepository
        {
            get
            {
                return _connectivityStepReferenceRepository ??
                       (_connectivityStepReferenceRepository =
                           new ConnectivityStepRefereceRepository(new AppDbContext()));
            }
        }

        public BaseRepository<ConnectivityLinkReferece> ConnectivityLinkRefereceRepository
        {
            get
            {
                return _connectivityLinkRefereceRepository ??
                       (_connectivityLinkRefereceRepository = new ConnectivityLinkRefereceRepository(new AppDbContext()));
            }
        }

        public BaseRepository<DecisionChart> DecisionChartRepository
        {
            get
            {
                return _decisionChartRepository ??
                       (_decisionChartRepository = new DecisionChartRepository(new AppDbContext()));
            }
        }

        public BaseRepository<DataDependency> DataDependencyRepository
        {
            get
            {
                return _dataDependencyRepository ??
                       (_dataDependencyRepository = new DataDependencyRepository(new AppDbContext()));
            }
        }

        public BaseRepository<ViewSourceMaster> ViewSourceMasterRepository
        {
            get
            {
                return _viewSourceMasterRepository ?? (_viewSourceMasterRepository = new ViewSourceMasterRepository(new AppDbContext()));
            }
        }

        public BaseRepository<WorkflowReferences> WorkflowReferencesRepository
        {
            get
            {
                return _workflowReferences ??
                       (_workflowReferences = new WorkflowReferencesRepository(new AppDbContext()));
            }
        }

        public BaseRepository<WorkflowTreeviewSecondTabDetails> WorkflowTreeviewSecondTabDetailsRepository
        {
            get
            {
                return _workflowTreeviewSecondTabDetails ??
                       (_workflowTreeviewSecondTabDetails =
                           new WorkflowTreeviewSecondTabDetailsRepository(new AppDbContext()));
            }
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        ~CodeVortoService()
        {
            Dispose(false);
        }

        private void Dispose(bool disposing)
        {
            if (disposing)
            {
            }
            // free native resources if there are any.
            if (_nativeResource == IntPtr.Zero) return;
            Marshal.FreeHGlobal(_nativeResource);
            _nativeResource = IntPtr.Zero;
        }
    }
}